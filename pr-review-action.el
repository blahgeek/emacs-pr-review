;;; pr-review-action.el --- Action part for pr-review  -*- lexical-binding: t; -*-

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
(require 'pr-review-input)
(require 'pr-review-api)
(require 'magit-section)
(require 'magit-diff)
(require 'browse-url)

(declare-function pr-review-refresh "pr-review")

(defconst pr-review--review-actions '("COMMENT" "APPROVE" "REQUEST_CHANGES")
  "Available actions for `pr-review-submit-review'.")

(defconst pr-review--merge-methods '("MERGE" "REBASE" "SQUASH")
  "Available methods for `pr-review-merge'.")

(defun pr-review--insert-quoted-content (body)
  "Insert BODY as quoted in markdown format."
  (when body
    (insert (replace-regexp-in-string "^" "> " body)
            "\n")))

(defun pr-review-reply-to-thread (&rest _)
  "Reply to current thread."
  (interactive)
  (let ((section (magit-current-section))
        reply-content)
    (when (pr-review--review-thread-item-section-p section)
      (setq reply-content (oref section body)
            section (oref section parent)))
    (when (pr-review--review-thread-section-p section)
      (pr-review--open-input-buffer
       "Reply to thread."
       (apply-partially #'pr-review--insert-quoted-content reply-content)
       (apply-partially #'pr-review--post-review-comment-reply
                        (alist-get 'id pr-review--pr-info)
                        (oref section top-comment-id))
       'refresh-after-exit))))

(declare-function pr-review-refresh "pr-review")

(defun pr-review-resolve-thread (&rest _)
  "Resolve or unresolve current thread."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (pr-review--review-thread-item-section-p section)
      (setq section (oref section parent)))
    (when (pr-review--review-thread-section-p section)
      (let ((resolved (oref section is-resolved))
            (thread-id (oref section value)))
        (when (y-or-n-p (format "Really %s this thread? "
                                (if resolved "unresolve" "resolve")))
          (pr-review--post-resolve-review-thread
           thread-id (not resolved))
          (pr-review-refresh))))))

(defun pr-review-comment (&rest _)
  "Post comment to this PR."
  (interactive)
  (let ((section (magit-current-section))
        reply-content)
    (when (or (pr-review--comment-section-p section)
              (pr-review--review-section-p section))
      (setq reply-content (oref section body)))
    (pr-review--open-input-buffer
     "Comment to PR."
     (apply-partially #'pr-review--insert-quoted-content reply-content)
     (apply-partially #'pr-review--post-comment
                      (alist-get 'id pr-review--pr-info))
     'refresh-after-exit)))


(defun pr-review--get-diff-line-info (pt)
  "Return (side . (filename . line)) for diff line at PT."
  (save-excursion
    (goto-char pt)
    (beginning-of-line)
    (let (prop)
      (cond
       ((setq prop (get-text-property (point) 'pr-review-diff-line-left))
        (cons "LEFT" prop))
       ((setq prop (get-text-property (point) 'pr-review-diff-line-right))
        (cons "RIGHT" prop))))))


(declare-function pr-review--insert-in-diff-pending-review-thread "pr-review-render")

(defun pr-review--add-pending-review-thread-exit-callback (orig-buffer review-thread body)
  "Exit callback for adding pending review thread.
ORIG-BUFFER is the original pr review buffer;
REVIEW-THREAD is the related thread;
BODY is the result text user entered."
  (setf (alist-get 'body review-thread) body)
  (when (buffer-live-p orig-buffer)
    (with-current-buffer orig-buffer
      (let ((inhibit-read-only t))
        (pr-review--insert-in-diff-pending-review-thread review-thread))
      (set-buffer-modified-p t)
      (push review-thread pr-review--pending-review-threads))))

(defun pr-review-add-pending-review-thread ()
  "Add pending review thread under current point (must be in a diff line).
When a region is active, the review thread is added for multiple lines."
  (interactive)
  (let* ((line-info (pr-review--get-diff-line-info
                     (if (use-region-p) (1- (region-end)) (point))))
         (start-line-info (when (use-region-p)
                            (pr-review--get-diff-line-info (region-beginning))))
         region-text
         review-thread)
    (when (equal line-info start-line-info)
      (setq start-line-info nil))
    (if (or (null line-info)
            (and start-line-info
                 (not (equal (cadr line-info) (cadr start-line-info)))))
        (message "Cannot add review thread at current point")
      (setq review-thread `((path . ,(cadr line-info))
                            (line . ,(cddr line-info))
                            (side . ,(car line-info))
                            (startLine . ,(cddr start-line-info))
                            (startSide . ,(car start-line-info))))
      (when (use-region-p)
        (setq region-text (replace-regexp-in-string
                           (rx line-start (any ?+ ?- ?\s)) ""
                           (buffer-substring-no-properties (region-beginning) (region-end))))
        (unless (string-suffix-p "\n" region-text)
          (setq region-text (concat region-text "\n"))))
      (pr-review--open-input-buffer
       "Start review thread."
       (when region-text
         (lambda ()
           (insert "```suggestion\n" region-text "```")
           (goto-char (point-min))))
       (apply-partially #'pr-review--add-pending-review-thread-exit-callback
                        (current-buffer)
                        review-thread))
      t)))

(defun pr-review-edit-pending-review-thread ()
  "Edit pending review thread under current point."
  (interactive)
  (when-let* ((review-thread (get-text-property (point) 'pr-review-pending-review-thread))
              (end (next-single-property-change (point) 'pr-review-pending-review-thread))
              (beg (previous-single-property-change end 'pr-review-pending-review-thread)))
    (let ((inhibit-read-only t))
      (delete-region beg end))
    (setq-local pr-review--pending-review-threads
                (delq review-thread pr-review--pending-review-threads))
    (pr-review--open-input-buffer
     "Edit review thread."
     (lambda ()
       (insert (alist-get 'body review-thread))
       (goto-char (point-min)))
     (apply-partially #'pr-review--add-pending-review-thread-exit-callback
                      (current-buffer)
                      review-thread))
    t))

(defun pr-review-edit-or-add-pending-review-thread ()
  "Edit pending review thread or add a new one, depending on the current point."
  (interactive)
  (or (pr-review-edit-pending-review-thread)
      (pr-review-add-pending-review-thread)))

(defun pr-review--submit-review-exit-callback (orig-buffer event body)
  "Exit callback for submitting reviews.
ORIG-BUFFER is the original pr review buffer;
EVENT is the review action user selected;
BODY is the result text user entered."
  (when (buffer-live-p orig-buffer)
    (with-current-buffer orig-buffer
      (pr-review--post-review (alist-get 'id pr-review--pr-info)
                              (alist-get 'headRefOid pr-review--pr-info)
                              event
                              (nreverse pr-review--pending-review-threads)
                              body)
      (setq-local pr-review--pending-review-threads nil))))

(defun pr-review-submit-review (event)
  "Submit review with pending review threads, with action EVENT.
When called interactively, user will be asked to choose an event."
  (interactive (list (completing-read "Select review action: "
                                      pr-review--review-actions
                                      nil 'require-match)))
  (pr-review--open-input-buffer
   (format "Submit review %s (%s threads)." event (length pr-review--pending-review-threads))
   nil
   (apply-partially #'pr-review--submit-review-exit-callback
                    (current-buffer) event)
   'refresh-after-exit
   'allow-empty))

(defun pr-review-merge (method)
  "Merge current PR with METHOD.
Available methods is `pr-review--merge-methods'.
Will confirm before sending the request."
  (interactive (list (completing-read "Select merge method: "
                                      pr-review--merge-methods
                                      nil 'require-match)))
  (when (y-or-n-p (format "Really merge this PR with method %s? " method))
    (pr-review--post-merge-pr (alist-get 'id pr-review--pr-info) method)
    (pr-review-refresh)))

(defun pr-review--close-or-reopen-action ()
  "Return the expected action if `pr-review-close-or-reopen' is called.
Maybe 'close or 'reopen or nil."
  (pcase (alist-get 'state pr-review--pr-info)
    ("CLOSED" 'reopen)
    ("OPEN" 'close)
    (_ nil)))

(defun pr-review-close-or-reopen ()
  "Close or re-open PR based on current state.
Will confirm before sending the request."
  (interactive)
  (pcase (alist-get 'state pr-review--pr-info)
    ("CLOSED" (when (y-or-n-p "Really re-open this PR? ")
                (pr-review--post-reopen-pr (alist-get 'id pr-review--pr-info))
                (pr-review-refresh)))
    ("OPEN" (when (y-or-n-p "Really close this PR? ")
              (pr-review--post-close-pr (alist-get 'id pr-review--pr-info))
              (pr-review-refresh)))
    (_
     (error "Cannot close or reopen PR in current state"))))

(defun pr-review-close-or-reopen-or-merge (action)
  "Close or re-open or merge based on ACTION.
Used for interactive selection one of them."
  (interactive (list (let ((actions pr-review--merge-methods))
                       (when-let ((close-or-reopen-action (pr-review--close-or-reopen-action)))
                         (setq actions
                               (append actions
                                       (list (upcase (symbol-name close-or-reopen-action))))))
                       (completing-read "Select action: "
                                        actions nil 'require-match))))
  (if (member action pr-review--merge-methods)
      (pr-review-merge action)
    (pr-review-close-or-reopen)))

(defun pr-review-edit-comment ()
  "Edit comment under current point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (-is-comment-section (pr-review--comment-section-p section))
              (updatable (oref section updatable))
              (id (oref section value))
              (body (oref section body)))
    (pr-review--open-input-buffer
     "Update comment."
     (lambda () (insert body))
     (apply-partially #'pr-review--update-comment id)
     'refresh-after-exit)))

(defun pr-review-edit-review ()
  "Edit review body under current point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (-is-review-section (pr-review--review-section-p section))
              (updatable (oref section updatable))
              (id (oref section value))
              (body (oref section body)))
    (pr-review--open-input-buffer
     "Update review."
     (lambda () (insert body))
     (apply-partially #'pr-review--update-review id)
     'refresh-after-exit)))

(defun pr-review-edit-review-comment ()
  "Edit review comment under current point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (-is-review-thread-item (pr-review--review-thread-item-section-p section))
              (updatable (oref section updatable))
              (id (oref section value))
              (body (oref section body)))
    (pr-review--open-input-buffer
     "Update review comment."
     (lambda () (insert body))
     (apply-partially #'pr-review--update-review-comment id)
     'refresh-after-exit)))

(defun pr-review-edit-pr-description ()
  "Edit pr description (body)."
  (interactive)
  (when-let* ((section (magit-current-section))
              (-is-description-section (pr-review--description-section-p section))
              (updatable (oref section updatable))
              (body (oref section body)))
    (pr-review--open-input-buffer
     "Update PR description."
     (lambda () (insert body))
     (apply-partially #'pr-review--update-pr-body (alist-get 'id pr-review--pr-info))
     'refresh-after-exit)))

(defun pr-review-edit-pr-title ()
  "Edit pr title."
  (interactive)
  (when-let* ((section (magit-current-section))
              (-is-root-section (pr-review--root-section-p section))
              (updatable (oref section updatable))
              (title (oref section title)))
    (pr-review--open-input-buffer
     "Update PR title."
     (lambda () (insert title))
     (apply-partially #'pr-review--update-pr-title (alist-get 'id pr-review--pr-info))
     'refresh-after-exit)))

(defun pr-review-view-file ()
  "View the full file under current point (must in some diff)."
  (interactive)
  (pcase-let ((`(,side . (,filepath . ,line)) (pr-review--get-diff-line-info (point))))
    (when (and side filepath line)
      (let* ((content (pr-review--fetch-file filepath
                                             (if (equal side "LEFT") 'base 'head)))
             (tempfile (make-temp-file (if (equal side "LEFT") "BASE~" "HEAD~")
                                       nil
                                       (concat "~" (file-name-nondirectory filepath))
                                       content)))
        (with-current-buffer (find-file-other-window tempfile)
          (goto-char (point-min))
          (forward-line (1- line)))))))

(defun pr-review-open-in-default-browser ()
  "Open current PR in default browser."
  (interactive)
  (browse-url-default-browser (alist-get 'url pr-review--pr-info)))

;; general dispatching functions, call other functions based on current context

(defun pr-review--review-thread-context-p (section)
  "Check whether SECTION is a review thread (or its children)."
  (or (pr-review--review-thread-section-p section)
      (pr-review--review-thread-item-section-p section)))

(defun pr-review--diff-context-p (section)
  "Check whether SECTION is a diff section (or its children)."
  (or (pr-review--diff-section-p section)
      (magit-hunk-section-p section)
      (magit-file-section-p section)
      (magit-module-section-p section)
      (get-text-property (point) 'pr-review-pending-review-thread)))

(defun pr-review-context-comment ()
  "Comment on current point.
Based on current context, may be:
reply to thread, post comment, add/edit review on diff."
  (interactive)
  (pcase (magit-current-section)
    ((pred pr-review--review-thread-context-p)
     (pr-review-reply-to-thread))
    ((pred pr-review--diff-context-p)
     (pr-review-edit-or-add-pending-review-thread))
    (_
     (pr-review-comment))))


(defun pr-review-context-action ()
  "Action on current point.
Based on current context, may be: resolve thread, submit review."
  (interactive)
  (pcase (magit-current-section)
    ((pred pr-review--review-thread-context-p)
     (pr-review-resolve-thread))
    ;; in diff, or has pending review threads
    ((or (pred pr-review--diff-context-p)
         (pred (lambda (_) pr-review--pending-review-threads)))
     (call-interactively #'pr-review-submit-review))
    (_
     (call-interactively #'pr-review-close-or-reopen-or-merge))))


(defun pr-review-context-edit ()
  "Edit on current point.
Based on current context, may be:
edit description, edit review comment, edit comment, edit pending diff review."
  (interactive)
  (pcase (magit-current-section)
    ((pred pr-review--description-section-p)
     (pr-review-edit-pr-description))
    ((pred pr-review--review-thread-item-section-p)
     (pr-review-edit-review-comment))
    ((pred pr-review--comment-section-p)
     (pr-review-edit-comment))
    ((pred pr-review--review-section-p)
     (pr-review-edit-review))
    ((pred pr-review--diff-context-p)
     (pr-review-edit-pending-review-thread))
    ((pred pr-review--root-section-p)
     (pr-review-edit-pr-title))
    (_
     (message "No action available in current context"))))


(defun pr-review--find-all-file-sections (section)
  "Recursively find all file sections in SECTION."
  (if (magit-file-section-p section)
      (list section)
    (mapcan #'pr-review--find-all-file-sections
            (oref section children))))

(defun pr-review-goto-file (filepath)
  "Goto section for FILEPATH in current buffer.
When called interactively, user can select filepath from list."
  (interactive (list (completing-read
                      "Goto file:"
                      (mapcar (lambda (section) (oref section value))
                              (pr-review--find-all-file-sections magit-root-section))
                      nil 'require-match)))
  (when-let ((section (seq-find (lambda (section) (equal (oref section value) filepath))
                                (pr-review--find-all-file-sections magit-root-section))))
    (push-mark)
    (goto-char (oref section start))
    (recenter)))

(defun pr-review-request-reviews (reviewer-logins)
  "Request reviewers for current PR, with a list of usernames REVIEWER-LOGINS.
This will override all existing reviewers (will clear all reviewers on empty).
When called interactively, user can select reviewers from list."
  (interactive
   (list
    (let ((assignable-users (pr-review--get-assignable-users)))
      (completing-read-multiple
       "Request review: "
       (mapcar (lambda (usr) (alist-get 'login usr)) assignable-users)
       nil 'require-match
       (string-join
        (mapcar (lambda (reviewer) (let-alist reviewer .requestedReviewer.login))
                (let-alist pr-review--pr-info .reviewRequests.nodes))
        ",")))))
  (let* ((assignable-users (pr-review--get-assignable-users))
         (ids (mapcar (lambda (login)
                        (let ((usr (seq-find (lambda (elem)
                                               (equal (alist-get 'login elem) login))
                                             assignable-users)))
                          (unless usr
                            (error "User %s not found" login))
                          (alist-get 'id usr)))
                      reviewer-logins)))
    (pr-review--post-request-reviews (alist-get 'id pr-review--pr-info) ids)
    (pr-review-refresh)))

(defun pr-review-goto-database-id (database-id)
  "Goto section with DATABASE-ID, which is used as the anchor in github urls."
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      (when-let ((match (text-property-search-forward
                         'magit-section database-id
                         (lambda (target prop-value)
                           (when (or (pr-review--review-section-p prop-value)
                                     (pr-review--comment-section-p prop-value)
                                     (pr-review--review-thread-item-section-p prop-value))
                             (equal (number-to-string (oref prop-value databaseId))
                                    target))))))
        (setq pos (prop-match-beginning match))))
    (when pos
      (goto-char pos)
      (recenter))))

(provide 'pr-review-action)
;;; pr-review-action.el ends here
