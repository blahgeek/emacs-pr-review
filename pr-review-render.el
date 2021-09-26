;;; pr-review-render.el --- Render part for pr-review  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'pr-review-common)

(defvar-local pr-review--diff-begin-point 0)

;; section classes
(defclass pr-review--review-section (magit-section)
  ((keymap :initform pr-review-review-section-map)
   (body :initform nil)
   (updatable :initform nil)))

(defclass pr-review--comment-section (magit-section)
  ((keymap :initform pr-review-comment-section-map)
   (body :initform nil)
   (updatable :initform nil)))

(defclass pr-review--diff-section (magit-section) ())
(defclass pr-review--check-section (magit-section) ())

(defclass pr-review--review-thread-section (magit-section)
  ((keymap :initform pr-review-review-thread-section-map)
   (top-comment-id :initform nil)
   (is-resolved :initform nil)))

(defclass pr-review--review-thread-item-section (magit-section)
  ((keymap :initform pr-review-review-thread-item-section-map)
   (body :initform nil)
   (updatable :initform nil)))

(defun pr-review--format-timestamp (str)
  "Convert and format timestamp STR from json."
  (format-time-string "%b %d, %Y, %H:%M" (date-to-time str)))

(defun pr-review--insert-link (title url)
  (insert-button title 'face 'pr-review-link-face
                 'action (lambda (_) (browse-url url))))

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
      (insert (string-replace "\r\n" "\n" body)
              " \n")
      (unless (eq major-mode lang-mode)
        (funcall lang-mode))
      (font-lock-ensure))

    (when (eq lang-mode 'markdown-mode)
      (let ((markdown-display-remote-images t)
            (markdown-max-image-size `(,(window-pixel-width) . nil)))
        (markdown-display-inline-images))
      ;; copy overlays to text properties with 'display property (images)
      (save-excursion
        (dolist (ol (overlays-in (point-min) (point-max)))
          (when-let ((display (overlay-get ol 'display)))
            (add-text-properties (overlay-start ol)
                                 (overlay-end ol)
                                 `(display ,display))))))

    ;; delete invisible texts
    (let (match)
      (beginning-of-buffer)
      (while (setq match (text-property-search-forward 'invisible))
        (let ((beg (prop-match-beginning match))
              (end (prop-match-end match)))
          (remove-text-properties beg end '(invisible nil)))))

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

(defun pr-review--insert-in-diff-pending-review-thread (pending-review-thread)
  (save-excursion
    (let (beg end)
      (let-alist pending-review-thread
        (when (pr-review--goto-diff-line .path .side .line)
          (forward-line)
          (setq beg (point))
          (insert (propertize (concat "> PENDING comment for "
                                      (if .startLine
                                          (format "%s:%s to %s:%s" .startSide .startLine .side .line)
                                        (format "%s:%s" .side .line))
                                      "\n")
                              'face 'pr-review-in-diff-pending-begin-face))
          (pr-review--insert-fontified .body 'markdown-mode)
          (insert (propertize " \n" 'face 'pr-review-in-diff-pending-end-face))
          (setq end (point))))
      (when beg
        (add-text-properties beg end `(
                                       pr-review-pending-review-thread ,pending-review-thread
                                       pr-review-pending-review-beg ,beg
                                       pr-review-pending-review-end ,end
                                       keymap ,pr-review-diff-section-map))))))

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
           'face 'pr-review-button-face
           'action (lambda (_) (pr-review--goto-section-with-value .id)))
          (insert (propertize "\n" 'face 'pr-review-in-diff-thread-title-face)))))))

(defun pr-review--insert-review-thread-section (top-comment review-thread)
  (magit-insert-section section (pr-review--review-thread-section
                                 (alist-get 'id review-thread)
                                 (eq t (alist-get 'isCollapsed review-thread)))
    (oset section top-comment-id (alist-get 'id top-comment))
    (oset section is-resolved (eq t (alist-get 'isResolved review-thread)))
    (magit-insert-heading
      (propertize
       (let-alist review-thread
         (concat
          .path (when .line (if .startLine
                                (format ":%s-%s" .startLine .line)
                              (format ":%s" .line)))
          (when (eq t .isResolved) " - RESOLVED")
          (when (eq t .isOutdated) " - OUTDATED")))
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
    (insert (propertize " \n" 'face 'pr-review-thread-diff-end-face))
    (mapc (lambda (cmt)
            (let-alist cmt
              (magit-insert-section item-section (pr-review--review-thread-item-section .id)
                (oset item-section updatable .viewerCanUpdate)
                (oset item-section body .body)
                (magit-insert-heading
                  (propertize (concat "@" .author.login)
                              'face 'pr-review-author-face)
                  " - "
                  (propertize (pr-review--format-timestamp .createdAt)
                              'face 'pr-review-timestamp-face)
                  " ::")
                (pr-review--insert-fontified .body 'markdown-mode 'fill-column)
                (insert "\n"))))
          (let-alist review-thread .comments.nodes))
    (insert-button "Reply to thread"
                   'face 'pr-review-button-face
                   'action 'pr-review-reply-to-thread)
    (insert "  ")
    (let ((resolved (eq t (alist-get 'isResolved review-thread)))
          (thread-id (alist-get 'id review-thread)))
      (insert-button (if resolved "Unresolve" "Resolve")
                     'face 'pr-review-button-face
                     'action 'pr-review-resolve-thread))
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
        (magit-insert-section section (pr-review--review-section .id)
          (oset section updatable .viewerCanUpdate)
          (oset section body .body)
          (magit-insert-heading
            "@" .author.login " REVIEW " .state " - "
            (pr-review--format-timestamp .createdAt))
          (unless (string-empty-p .body)
            (pr-review--insert-fontified .body 'markdown-mode 'fill-column))
          (insert "\n")
          (dolist (top-comment-and-review-thread top-comment-and-review-thread-list)
            (apply 'pr-review--insert-review-thread-section top-comment-and-review-thread))
          (when top-comment-and-review-thread-list
            (insert "\n")))))))

(defun pr-review--insert-comment-section (cmt)
  (let-alist cmt
    (magit-insert-section section (pr-review--comment-section .id)
      (oset section updatable .viewerCanUpdate)
      (oset section body .body)
      (magit-insert-heading
        "@" .author.login " COMMENTED - "
        (pr-review--format-timestamp .createdAt))
      (pr-review--insert-fontified .body 'markdown-mode 'fill-column)
      (insert "\n"))))

(defun pr-review--insert-reviewers-info (pr-info)
  (let ((groups (make-hash-table :test 'equal)))
    (when-let ((review-requests (let-alist pr-info .reviewRequests.node)))
      (when (length> review-requests 0)
        (puthash "PENDING" (mapcar (lambda (review-request)
                                     (let-alist review-request .requestedReviewer.login))
                                   review-requests)
                 groups)))
    (mapc (lambda (opinionated-review)
            (let-alist opinionated-review
              (push .author.login (gethash .state groups))))
          (let-alist pr-info .latestOpinionatedReviews.nodes))
    (maphash (lambda (status users)
               (insert (propertize status 'face 'pr-review-state-face)
                       ": "
                       (mapconcat (lambda (user)
                                    (propertize (concat "@" user) 'face 'pr-review-author-face))
                                  users ", ")
                       "\n"))
             groups)))

(defun pr-review--insert-check-section (status-check-rollup)
  (magit-insert-section (pr-review--check-section)
    (magit-insert-heading (concat "Check status - " (alist-get 'state status-check-rollup)))
    (mapc (lambda (node)
            (let-alist node
              (pcase .__typename
                ("CheckRun"
                 (insert (concat "- "
                                 (propertize .name 'face 'pr-review-author-face)
                                 ": "
                                 (propertize .status 'face 'pr-review-state-face)
                                 (when .conclusion
                                   (propertize (concat " - " .conclusion)
                                               'face 'pr-review-state-face))
                                 (when .title
                                   (concat " - " .title))
                                 "\n")))
                ("StatusContext"
                 (insert (concat "- "
                                 (propertize .context 'face 'pr-review-author-face)
                                 ": "
                                 (propertize .state 'face 'pr-review-state-face)
                                 (when .description
                                   (concat " - " .description))
                                 " "))
                 (when .targetUrl
                   (pr-review--insert-link "Details" .targetUrl))
                 (insert "\n")))))
          (let-alist status-check-rollup .contexts.nodes))
    (insert "\n")))

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
              "\n")
      (pr-review--insert-reviewers-info pr)
      (insert "\n")
      (pr-review--insert-fontified .body 'markdown-mode 'fill-column)
      (insert "\n"))
    (dolist (review-or-comment review-or-comments)
      (pcase (cdr review-or-comment)
        ('review
         (pr-review--insert-review-section (car review-or-comment) top-comment-id-to-review-thread))
        ('comment
         (pr-review--insert-comment-section (car review-or-comment)))))
    (pr-review--insert-check-section
     (let-alist (nth 0 (let-alist pr .commits.nodes)) .commit.statusCheckRollup))
    (magit-insert-section (pr-review--diff-section)
      (magit-insert-heading
        (let-alist pr
          (format "Files changed (%s files; %s additions, %s deleletions)"
                  (length .files.nodes)
                  (apply '+ (mapcar (lambda (x) (alist-get 'additions x)) .files.nodes))
                  (apply '+ (mapcar (lambda (x) (alist-get 'deletions x)) .files.nodes)))))
      (pr-review--insert-diff diff)
      (insert "\n")
      (dolist (event '("COMMENT" "APPROVE" "REQUEST_CHANGES"))
        (insert-button event 'face 'pr-review-button-face
                       'action (lambda (_) (pr-review-submit-review event)))
        (insert " ")))
    (mapc 'pr-review--insert-in-diff-review-thread-link
          (let-alist pr .reviewThreads.nodes))))


(provide 'pr-review-render)
;;; pr-review-render.el ends here
