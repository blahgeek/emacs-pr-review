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
  '((t :inherit font-lock-keyword-face))
  "Face used for author names."
  :group 'pr-review)

(defface pr-review-timestamp-face
  '((t :slant italic))
  "Face used for timestamps."
  :group 'pr-review)

(defface pr-review-branch-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for branchs."
  :group 'pr-review)

(defface pr-review-label-face
  '((t :box t :foregroud "black"))
  "Face used for labels."
  :group 'pr-review)

(defface pr-review-thread-item-title-face
  '((t :inherit bold))
  "Face used for title of review thread item."
  :group 'pr-review)

(defface pr-review-thread-diff-begin-face
  '((t :underline t :extend t :inherit font-lock-comment-face))
  "Face used for the beginning of thread diff hunk."
  :group 'pr-review)

(defface pr-review-thread-diff-end-face
  '((t :overline t :extend t :inherit font-lock-comment-face))
  "Face used for the beginning of thread diff hunk."
  :group 'pr-review)

(defface pr-review-in-diff-thread-title-face
  '((t :inherit font-lock-comment-face))
  "Face used for the title of the in-diff thread title."
  :group 'pr-review)

(defface pr-review-link-face
  '((t :inherit link))
  "Face used for links."
  :group 'pr-review)


(defvar-local pr-review--diff-begin-point 0)
(defvar-local pr-review--pr-node-id nil)

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
    (setq-local pr-review--diff-begin-point beg)

    (pr-review--insert-fontified diff 'diff-mode)
    (goto-char beg)
    (magit-wash-sequence (apply-partially 'magit-diff-wash-diff '()))

    (goto-char beg)
    (forward-line -1)
    (let (filename left right current-left-right)
      (while (zerop (forward-line))
        (let ((section-data (get-text-property (point) 'magit-section)))
          (when (magit-file-section-p section-data)
            (setq filename (oref section-data value))
            (set-text-properties 0 (length filename) nil filename))
          (when (and (magit-hunk-section-p section-data)
                     (magit-section-position-in-heading-p))
            (setq left (car (oref section-data from-range))
                  right (car (oref section-data to-range))))
          (pcase (char-after)
            ('?\s (setq current-left-right (cons left right)
                        left (1+ left)
                        right (1+ right)))
            ('?- (setq current-left-right (cons left nil)
                       left (1+ left)))
            ('?+ (setq current-left-right (cons nil right)
                       right (1+ right)))
            (_ (setq current-left-right nil)))
          (when (car current-left-right)
            (add-text-properties
             (point) (1+ (point))
             `(pr-review-diff-line-left ,(cons filename (car current-left-right)))))
          (when (cdr current-left-right)
            (add-text-properties
             (point) (1+ (point))
             `(pr-review-diff-line-right ,(cons filename (cdr current-left-right)))))
          )))))

(defun pr-review--find-section-with-value (value)
  "Find and return the magit-section object matching VALUE."
  (save-excursion
    (beginning-of-buffer)
    (when-let ((match (text-property-search-forward
                       'magit-section value
                       (lambda (target prop-value)
                         (and prop-value
                              (magit-section-p prop-value)
                              (equal (oref prop-value value) target))))))
      (get-text-property (prop-match-beginning match) 'magit-section))))

(defun pr-review--goto-section-with-value (value)
  (when-let ((section (pr-review--find-section-with-value value)))
    (goto-char (oref section start))))

(defun pr-review--goto-diff-line (filepath diffside line)
  "Goto diff line for FILEPATH, DIFFSIDE (string, left or right) and LINE, return t on success."
  (goto-char pr-review--diff-begin-point)
  (when-let ((match (text-property-search-forward
                     (if (equal diffside "LEFT")
                         'pr-review-diff-line-left
                       'pr-review-diff-line-right)
                     (cons filepath line)
                     t)))
    (goto-char (prop-match-beginning match))
    t))

(defun pr-review--insert-in-diff-review-thread-link (review-thread)
  "Insert REVIEW-THREAD inside the diff section."
  (let-alist review-thread
    (when (not .isOutdated)
      (save-excursion
        (when (pr-review--goto-diff-line
               .path .diffSide .line)
          (forward-line)
          (insert
           (propertize
            (concat (format "> %s comments from " (length .comments.nodes))
                    (string-join
                     (seq-uniq
                      (mapcar (lambda (cmt)
                                (concat "@" (alist-get 'login (alist-get 'author cmt))))
                              .comments.nodes))
                     ", ")
                    (when .isResolved " - RESOLVED")
                    "  ")
            'face 'pr-review-in-diff-thread-title-face))
          (insert-button
           "Go to thread"
           'face 'pr-review-link-face
           'action (lambda (_) (pr-review--goto-section-with-value .id)))
          (insert (propertize "\n" 'face 'pr-review-in-diff-thread-title-face)))))))

(defun pr-review--insert-review-thread-section (top-comment review-thread)
  (magit-insert-section (pr-review-review-thread
                         (alist-get 'id review-thread)
                         (eq t (alist-get 'isCollapsed review-thread)))
    (magit-insert-heading
      (propertize
       (concat
        (alist-get 'path top-comment)
        (when (eq t (alist-get 'isResolved review-thread)) " - RESOLVED")
        (when (eq t (alist-get 'isOutdated review-thread)) " - OUTDATED"))
       'face 'magit-section-secondary-heading))
    (insert (propertize " \n" 'face 'pr-review-thread-diff-begin-face))
    (let (beg end)
      (setq beg (point))
      (pr-review--insert-fontified (alist-get 'diffHunk top-comment) 'diff-mode)
      (setq end (point))
      (make-button beg end
                   'face nil
                   'help-echo "Click to go to the line in diff."
                   'action (lambda (_) (let-alist review-thread
                                         (pr-review--goto-diff-line .path .diffSide .line)))))
    (insert "\n" (propertize " \n" 'face 'pr-review-thread-diff-end-face))
    (mapc (lambda (cmt)
            (let-alist cmt
              (insert (propertize (concat "@" .author.login)
                                  'face 'pr-review-author-face)
                      " - "
                      (propertize (pr-review--format-timestamp .createdAt)
                                  'face 'pr-review-timestamp-face)
                      " ::\n")
              (pr-review--insert-fontified .body 'markdown-mode 'fill-column)
              (insert "\n\n")))
          (let-alist review-thread .comments.nodes))
    (insert-button "Reply to thread"
                   'face 'pr-review-link-face
                   'action (lambda (_)
                             (pr-review--open-comment-input-buffer
                              "Reply to thread."
                              nil
                              (apply-partially 'pr-review--post-review-comment-reply
                                               pr-review--pr-node-id
                                               (alist-get 'id top-comment)))))
    (insert " ")
    (let ((resolved (eq t (alist-get 'isResolved review-thread)))
          (thread-id (alist-get 'id review-thread)))
      (insert-button (if resolved "Unresolve" "Resolve")
                     'face 'pr-review-link-face
                     'action (lambda (_)
                               (when (y-or-n-p (format "Really %s this thread?"
                                                       (if resolved "unresolve" "resolve")))
                                 (pr-review--post-resolve-review-thread
                                  thread-id (not resolved))))))
    (insert "\n\n")))

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
      (insert (propertize .baseRefName 'face 'pr-review-branch-face)
              " <- "
              (propertize .headRefName 'face 'pr-review-branch-face)
              "  "
              (mapconcat (lambda (label)
                           (propertize (alist-get 'name label)
                                       'face
                                       `(:background ,(concat "#" (alist-get 'color label))
                                         :inherit pr-review-label-face)))
                         .labels.nodes " ")
              "\n")
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
      (magit-insert-heading
        (let-alist pr
          (format "Files changed (%s files; %s additions, %s deleletions)"
                  (length .files.nodes)
                  (apply '+ (mapcar (lambda (x) (alist-get 'additions x)) .files.nodes))
                  (apply '+ (mapcar (lambda (x) (alist-get 'deletions x)) .files.nodes)))))
      (pr-review--insert-diff diff))
    (mapc 'pr-review--insert-in-diff-review-thread-link
          (let-alist pr .reviewThreads.nodes))))


;;  --- comment input related ---

(defvar-local pr-review--comment-input-saved-window-config nil)
(defvar-local pr-review--comment-input-exit-callback nil)

(defun pr-review-comment-input-abort ()
  "Abort current comment input buffer, discard content."
  (interactive)
  (unless pr-review-comment-input-mode (error "Invalid mode"))
  (let ((saved-window-config pr-review--comment-input-saved-window-config))
    (kill-buffer)
    (when saved-window-config
      (unwind-protect
          (set-window-configuration saved-window-config)))))

(defun pr-review-comment-input-exit ()
  "Apply content and exit current comment input buffer."
  (interactive)
  (unless pr-review-comment-input-mode (error "Invalid mode"))
  (when pr-review--comment-input-exit-callback
    (funcall pr-review--comment-input-exit-callback (buffer-string)))
  (pr-review-comment-input-abort))

(defvar pr-review-comment-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'pr-review-comment-input-exit)
    (define-key map "\C-c\C-k" 'pr-review-comment-input-abort)
    map))

(define-minor-mode pr-review-comment-input-mode
  "Minor mode for PR Review comment input buffer."
  :lighter " PrReviewCommentInput")

(defun pr-review--open-comment-input-buffer (description open-callback exit-callback)
  "Open a comment buffer for user input with DESCRIPTION,
OPEN-CALLBACK is called when the buffer is opened,
EXIT-CALLBACK is called when the buffer is exit (not abort),
both callbacks are called inside the comment buffer."
  (with-current-buffer (generate-new-buffer "*pr-review comment input*")
    (markdown-mode)
    (pr-review-comment-input-mode)

    (setq-local
     header-line-format (concat description " "
                                (substitute-command-keys
                                 (concat "Confirm with `\\[pr-review-comment-input-exit]' or "
                                         "abort with `\\[pr-review-comment-input-abort]'")))
     pr-review--comment-input-saved-window-config (current-window-configuration)
     pr-review--comment-input-exit-callback exit-callback)

    (when open-callback
      (funcall open-callback))

    (switch-to-buffer-other-window (current-buffer))))

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
           (concat pr-review-bin-dir "graphql/" (symbol-name name) ".graphql"))
          (setq content (buffer-string)))
        (push (cons name content) pr-review--graphql-cache)
        content)))

(defun pr-review--execute-graphql (name variables)
  (let-alist (ghub-graphql (pr-review--get-graphql name)
                           variables
                           :auth pr-review-ghub-auth-name)
    (when .errors
      (error "Error while making graphql request %s: %s: %s"
             name .errors.type .errors.message))
    .data))

(defun pr-review--fetch-pr-info (repo-owner repo-name pr-id)
  (let-alist (pr-review--execute-graphql
              'get-pull-request
              `((repo_owner . ,repo-owner)
                (repo_name . ,repo-name)
                (pr_id . ,(if (numberp pr-id)
                              pr-id
                            (string-to-number pr-id)))))
    .repository.pullRequest))

(defun pr-review--fetch-pr-diff (repo-owner repo-name pr-id)
  (let ((res (ghub-request "GET"
                           (format "/repos/%s/%s/pulls/%s" repo-owner repo-name pr-id)
                           '()
                           :headers '(("Accept" . "application/vnd.github.v3.diff"))
                           :reader 'ghub--decode-payload
                           :auth pr-review-ghub-auth-name)))
    (concat res "\n")))  ;; don't why, just need an extra new line

(defun pr-review--post-review-comment-reply (pr-node-id top-comment-id body)
  (let (res review-id)
    (setq res (let-alist (pr-review--execute-graphql
                          'add-review-comment-reply
                          `((input . ((pullRequestId . ,pr-node-id)
                                      (inReplyTo . ,top-comment-id)
                                      (body . ,body)))))
                .addPullRequestReviewComment.comment))
    (unless (equal 1 (let-alist res (length .pullRequestReview.comments.nodes)))
      (error "Error while adding review comment reply, abort"))
    (setq review-id (let-alist res .pullRequestReview.id))
    (setq res (let-alist (pr-review--execute-graphql
                          'submit-review
                          `((input . ((pullRequestReviewId . ,review-id)
                                      (event . "COMMENT")))))
                .submitPullRequestReview.pullRequestReview))
    (unless (equal review-id (alist-get 'id res))
      (error "Error while submitting review comment reply"))))

(defun pr-review--post-resolve-review-thread (review-thread-id resolve-or-unresolve)
  (pr-review--execute-graphql (if resolve-or-unresolve
                                  'resolve-review-thread
                                'unresolve-review-thread)
                              `((input . ((threadId . ,review-thread-id))))))


(defun pr-review-open-parsed (repo-owner repo-name pr-id)
  (let ((pr-info (pr-review--fetch-pr-info repo-owner repo-name pr-id))
        (pr-diff (pr-review--fetch-pr-diff repo-owner repo-name pr-id))
        (buffer-name (format "*pr-review %s/%s/%s*" repo-owner repo-name pr-id)))
    (with-current-buffer (get-buffer-create buffer-name)
      (unless (eq major-mode 'pr-review-mode)
        (pr-review-mode))
      (setq pr-review--pr-node-id (alist-get 'id pr-info))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pr-review--insert-pr pr-info pr-diff))
      (beginning-of-buffer))
    (switch-to-buffer-other-window buffer-name)))


(provide 'pr-review)
;;; pr-review.el ends here
