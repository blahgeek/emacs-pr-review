;;; pr-review.el --- Review github PR in emacs.    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yikai Zhao

;; Author: Yikai Zhao <yikai@z1k.dev>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(require 'subr-x)
(require 'magit-section)
(require 'magit-diff)

(defgroup pr-review nil "Pr review.")

(defface pr-review-title-face
  '((t :inherit outline-1))
  "Face used for title."
  :group 'pr-review)

(defface pr-review-state-face
  '((t :inherit bold))
  "Face used for state (e.g. MERGED)."
  :group 'pr-review)

(defface pr-review-author-face
  '((t :inherit default))
  "Face used for author names."
  :group 'pr-review)

(defface pr-review-timestamp-face
  '((t :inherit 'italic))
  "Face used for timestamps."
  :group 'pr-review)

(defface pr-review-thread-item-title-face
  '((t :inherit 'bold))
  "Face used for title of review thread item."
  :group 'pr-review)

(defface pr-review-thread-diff-begin-face
  '((t :underline t :extend t :inherit 'font-lock-comment-face))
  "Face used for the beginning of thread diff hunk."
  :group 'pr-review)

(defface pr-review-thread-diff-end-face
  '((t :overline t :extend t :inherit 'font-lock-comment-face))
  "Face used for the beginning of thread diff hunk."
  :group 'pr-review)


(defun pr-review--format-timestamp (str)
  "Convert and format timestamp STR from json."
  (format-time-string "%b %d, %Y, %H:%M" (date-to-time str)))

  ;; Remove 'diff-context face, allow section highlighting in diff
  ;; but have some wierd effect, because other faces still have different backgrounds

  ;; (let (match)
  ;;   (while (setq match (text-property-search-forward
  ;;                       'face 'diff-context
  ;;                       (lambda (x face-prop)  ;; face-prop may be a list
  ;;                         (or (eq x face-prop)
  ;;                             (and (listp face-prop)
  ;;                                  (memq x face-prop))))))
  ;;     (let* ((beg (prop-match-beginning match))
  ;;            (end (prop-match-end match))
  ;;            (face (get-text-property beg 'face)))
  ;;       (if (listp face)
  ;;           (add-text-properties beg end
  ;;                                `(face ,(delete 'diff-context face)))
  ;;         (remove-text-properties beg end '(face nil))))))

(defun pr-review--fontify (body lang-mode &optional fill-column)
  (with-current-buffer
      (get-buffer-create (format " *pr-review-fontification:%s*" lang-mode))
    (let ((inhibit-modification-hooks nil)
          (diff-font-lock-syntax 'hunk-also))
      (erase-buffer)
      (insert (string-replace "\r\n" "\n" body) " ")
      (unless (eq major-mode lang-mode)
        (funcall lang-mode))
      (font-lock-ensure))

    (when (eq lang-mode 'diff-mode)
      (save-excursion
        (dolist (ol (overlays-in (point-min) (point-max)))
          (when-let ((_ (eq (overlay-get ol 'diff-mode) 'syntax))
                     (face (overlay-get ol 'face)))
            (add-face-text-property (overlay-start ol)
                                    (overlay-end ol)
                                    face)))
        (beginning-of-buffer)
        (remove-overlays (point-min) (point-max) 'diff-mode 'syntax)))

    (when fill-column
      (fill-region (point-min) (point-max)))

    (buffer-string)))


(defun pr-review--insert-fontified (body lang-mode &optional fill-column)
  (insert (pr-review--fontify body lang-mode fill-column)))


(defun pr-review--insert-diff (diff)
  (let ((beg (point)))
    ;; (insert diff)
    (pr-review--insert-fontified diff 'diff-mode)
    (goto-char beg)
    (magit-wash-sequence (apply-partially 'magit-diff-wash-diff '()))))


(defun pr-review--insert-review-thread-section (top-comment review-thread)
  (magit-insert-section (pr-review-review-thread
                         nil (eq t (alist-get 'isCollapsed review-thread)))
    (magit-insert-heading
      (propertize
       (concat
        (alist-get 'path top-comment)
        (when (eq t (alist-get 'isResolved review-thread)) " - RESOLVED"))
       'face 'magit-section-secondary-heading))
    (insert (propertize " \n" 'face 'pr-review-thread-diff-begin-face))
    (pr-review--insert-fontified (alist-get 'diffHunk top-comment) 'diff-mode)
    (insert "\n" (propertize " \n" 'face 'pr-review-thread-diff-end-face))
    (mapc (lambda (cmt)
            (let-alist cmt
              (insert (propertize
                       (concat (propertize (concat "@" .author.login)
                                           'face 'pr-review-author-face)
                               " - "
                               (propertize (pr-review--format-timestamp .createdAt)
                                           'face 'pr-review-timestamp-face)
                               " ::\n")
                       'face 'pr-review-thread-item-title-face))
              (pr-review--insert-fontified .body 'markdown-mode 'fill-column)
              (insert "\n\n")))
          (let-alist review-thread .comments.nodes))))

(defun pr-review--insert-review-section (review top-comment-id-to-review-thread)
  (let* ((review-comments (let-alist review .comments.nodes))
         (top-comment-and-review-thread-list
          (delq nil (mapcar (lambda (cmt)
                              (when-let* ((id (alist-get 'id cmt))
                                          (review-thread (gethash id top-comment-id-to-review-thread)))
                                (list cmt review-thread)))
                            review-comments))))
    (let-alist review
      (when (or top-comment-and-review-thread-list (not (string-empty-p .body)))
        (magit-insert-section (pr-review-review)
          (magit-insert-heading
            "@" .author.login " REVIEW " .state " - "
            (pr-review--format-timestamp .createdAt))
          (unless (string-empty-p .body)
            (pr-review--insert-fontified .body 'markdown-mode 'fill-column)
            (insert "\n"))
          (insert "\n")
          (dolist (top-comment-and-review-thread top-comment-and-review-thread-list)
            (apply 'pr-review--insert-review-thread-section top-comment-and-review-thread)))))))

(defun pr-review--insert-comment-section (cmt)
  (let-alist cmt
    (magit-insert-section (pr-review-comment)
      (magit-insert-heading
        "@" .author.login " COMMENTED - "
        (pr-review--format-timestamp .createdAt))
      (pr-review--insert-fontified .body 'markdown-mode 'fill-column)
      (insert "\n\n"))))

(defun pr-review--build-top-comment-id-to-review-thread-map (pr)
  (let ((res (make-hash-table :test 'equal)))
    (mapc (lambda (review-thread)
            (let* ((comments (let-alist review-thread .comments.nodes))
                   (top-comment (elt comments 0))
                   (top-comment-id (alist-get 'id top-comment)))
              (puthash top-comment-id review-thread res)))
          (let-alist pr .reviewThreads.nodes))
    res))

(defun pr-review--insert-pr (pr diff)
  (let ((top-comment-id-to-review-thread
         (pr-review--build-top-comment-id-to-review-thread-map pr))
        (review-or-comments
          (append
           (mapcar (lambda (x) (cons x 'review)) (let-alist pr .reviews.nodes))
           (mapcar (lambda (x) (cons x 'comment)) (let-alist pr .comments.nodes)))))
    (sort review-or-comments (lambda (a b) (string< (alist-get 'createdAt (car a))
                                                    (alist-get 'createdAt (car b)))))
    (let-alist pr
      (insert (propertize .title 'face 'pr-review-title-face) "\n\n")
      (insert (propertize .state 'face 'pr-review-state-face)
              " - "
              (propertize (concat "@" .author.login) 'face 'pr-review-author-face)
              " - "
              (propertize (pr-review--format-timestamp .createdAt) 'face 'pr-review-timestamp-face)
              "\n\n")
      (pr-review--insert-fontified .body 'markdown-mode 'fill-column)
      (insert "\n\n"))
    (dolist (review-or-comment review-or-comments)
      (pcase (cdr review-or-comment)
        ('review
         (pr-review--insert-review-section (car review-or-comment) top-comment-id-to-review-thread))
        ('comment
         (pr-review--insert-comment-section (car review-or-comment)))))
    (magit-insert-section (pr-review-diff)
      (magit-insert-heading "Changed files")
      (pr-review--insert-diff diff))))


;;  --- mode related ---

(defvar pr-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    map))

(define-derived-mode pr-review-mode magit-section-mode "PrReview"
  :interactive nil
  :group 'pr-review
  (use-local-map pr-review-mode-map)
  (when (fboundp 'evil-define-key)
    (evil-define-key '(normal motion) 'local
      (kbd "TAB") 'magit-section-toggle)))

;;  --- API related ---

(defvar pr-review-ghub-auth-name 'emacs-pr-review)
(defvar pr-review-bin-dir (file-name-directory load-file-name))

(defvar pr-review--graphql-cache '())

(defun pr-review--get-graphql (name)
  "Get graphql content for NAME (symbol), cached."
  (or (alist-get name pr-review--graphql-cache)
      (let (content)
        (with-temp-buffer
          (insert-file-contents-literally
           (concat pr-review-bin-dir (symbol-name name) ".graphql"))
          (setq content (buffer-string)))
        (push (cons name content) pr-review--graphql-cache)
        content)))

(defun pr-review--fetch-pr-info (repo-owner repo-name pr-id)
  (let-alist (ghub-graphql
              (pr-review--get-graphql 'get-pull-request)
              `((repo_owner . ,repo-owner)
                (repo_name . ,repo-name)
                (pr_id . ,(if (numberp pr-id)
                              pr-id
                            (string-to-number pr-id))))
              :auth pr-review-ghub-auth-name)
    .data.repository.pullRequest))

(defun pr-review--fetch-pr-diff (repo-owner repo-name pr-id)
  (let ((res (ghub-request "GET"
                           (format "/repos/%s/%s/pulls/%s" repo-owner repo-name pr-id)
                           '()
                           :headers '(("Accept" . "application/vnd.github.v3.diff"))
                           :reader 'ghub--decode-payload
                           :auth pr-review-ghub-auth-name)))
    (concat res "\n")))  ;; don't why, just need an extra new line

(defun pr-review-open-parsed (repo-owner repo-name pr-id)
  (let ((pr-info (pr-review--fetch-pr-info repo-owner repo-name pr-id))
        (pr-diff (pr-review--fetch-pr-diff repo-owner repo-name pr-id))
        (buffer-name (format "*pr-review %s/%s/%s*" repo-owner repo-name pr-id)))
    (with-current-buffer (get-buffer-create buffer-name)
      (unless (eq major-mode 'pr-review-mode)
        (pr-review-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pr-review--insert-pr pr-info pr-diff))
      (beginning-of-buffer))
    (switch-to-buffer-other-window buffer-name)))


(provide 'pr-review)
;;; pr-review.el ends here
