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
(require 'magit-log)
(require 'magit-diff)
(require 'browse-url)

(declare-function pr-review-refresh "pr-review")

(defconst pr-review--review-actions '("COMMENT" "APPROVE" "REQUEST_CHANGES")
  "Available actions for `pr-review-submit-review'.")

(defconst pr-review--merge-methods '("MERGE" "REBASE" "SQUASH")
  "Available methods for `pr-review-merge'.")

(defconst pr-review--subscription-states '("IGNORED" "SUBSCRIBED" "UNSUBSCRIBED")
  "Available states for `pr-review-update-subscription'.")

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
      (unless reply-content
        (setq reply-content (oref section body)))
      (pr-review--open-input-buffer
       "Reply to thread."
       (apply-partially #'pr-review--insert-quoted-content reply-content)
       (apply-partially #'pr-review--post-thread-reply
                        (alist-get 'id pr-review--pr-info)
                        (oref section reply-id))
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
    (let (prop side)
      (cond
       ((setq prop (get-text-property (point) 'pr-review-diff-line-left))
        (setq side "LEFT"))
       ((setq prop (get-text-property (point) 'pr-review-diff-line-right))
        (setq side "RIGHT")))
      (when side
        (cons side (let-alist prop
                     (cons .path .line)))))))


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

(cl-defgeneric pr-review--get-review-thread-input-at-current-point ()
  "Return review thread input under current point or region.
Must be in a diff line, return nil if not.
Result should be an alist, where 'body will be added addtionally and passed into pr-review--post-review."
  (let* ((line-info (pr-review--get-diff-line-info
                     (if (use-region-p) (1- (region-end)) (point))))
         (start-line-info (when (use-region-p)
                            (pr-review--get-diff-line-info (region-beginning))))
         result)
    (when (equal line-info start-line-info)
      (setq start-line-info nil))
    (unless (or (null line-info)
            (and start-line-info
                 (not (equal (cadr line-info) (cadr start-line-info)))))
      (setq result `((path . ,(cadr line-info))
                     (line . ,(cddr line-info))
                     (side . ,(car line-info))))
      (when start-line-info
        (setq result (append `((startLine . ,(cddr start-line-info))
                               (startSide . ,(car start-line-info)))
                             result)))
      result)))

(defun pr-review-add-pending-review-thread ()
  "Add pending review thread under current point (must be in a diff line).
When a region is active, the review thread is added for multiple lines."
  (interactive)
  (let* (region-text
         (review-thread (pr-review--get-review-thread-input-at-current-point)))
    (if (not review-thread)
        (message "Cannot add review thread at current point")
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
  (or (and (not (use-region-p))  ;; if region is active, always add instead of edit
           (pr-review-edit-pending-review-thread))
      (pr-review-add-pending-review-thread)))

(defun pr-review--submit-review-exit-callback (orig-buffer event body)
  "Exit callback for submitting reviews.
ORIG-BUFFER is the original pr review buffer;
EVENT is the review action user selected;
BODY is the result text user entered."
  (when (buffer-live-p orig-buffer)
    (with-current-buffer orig-buffer
      (let ((cleaned-threads (mapcar (lambda (thread)
                                       (assq-delete-all '-gh-compat-info (copy-alist thread)))
                                     pr-review--pending-review-threads)))
        (pr-review--post-review (alist-get 'id pr-review--pr-info)
                                (pr-review--current-commit-head)
                                event
                                (nreverse cleaned-threads)
                                body))
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
Maybe \='close or \='reopen or nil."
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

(defun pr-review-toggle-draft ()
  "Toggle the draft state of the current PR.
If the PR is a draft, mark it ready for review.
Otherwise, convert it back to a draft.
Will confirm before sending the request."
  (interactive)
  (unless (equal (alist-get 'state pr-review--pr-info) "OPEN")
    (user-error "Cannot toggle draft state on a PR that is not open"))
  (let ((id (alist-get 'id pr-review--pr-info))
        (is-draft (eq t (alist-get 'isDraft pr-review--pr-info))))
    (if is-draft
        (when (y-or-n-p "Mark this PR as ready for review? ")
          (pr-review--post-mark-ready-for-review id)
          (pr-review-refresh))
      (when (y-or-n-p "Convert this PR back to a draft? ")
        (pr-review--post-convert-to-draft id)
        (pr-review-refresh)))))

(cl-defmethod pr-review-general-interactive-action ()
  "Close or re-open or merge, prompt to interactive select the action."
  (let ((action (let ((actions pr-review--merge-methods))
                  (when-let ((close-or-reopen-action (pr-review--close-or-reopen-action)))
                    (setq actions
                          (append actions
                                  (list (upcase (symbol-name close-or-reopen-action))))))
                  (completing-read "Select action: "
                                   actions nil 'require-match))))
    (if (member action pr-review--merge-methods)
        (pr-review-merge action)
      (pr-review-close-or-reopen))))

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

(defun pr-review-edit-thread-item ()
  "Edit thread comment under current point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (-is-valid-section (pr-review--review-thread-context-p section))
              (updatable (oref section updatable))
              (id (or
                   ;; special case: gitlab review-thread-section, use update-id instead of value, because value is the discussion id
                   (and (pr-review--review-thread-section-p section)
                        (oref section update-id))
                   (oref section value)))
              (body (oref section body)))
    (pr-review--open-input-buffer
     "Update review comment."
     (lambda () (insert body))
     (apply-partially #'pr-review--update-thread-item id)
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

(defun pr-review--make-temp-file (head-or-base filepath content)
  (make-temp-file (concat (upcase (symbol-name head-or-base)) "~")
                  nil
                  (concat "~" (file-name-nondirectory filepath))
                  content))

(defun pr-review-view-file (head-or-base filepath &optional line)
  "View the full file content in a temporary buffer.
By default, view the file under current point (must in some diff).
When invoked with prefix, prompt for head-or-base and filepath."
  (interactive
   (let (head-or-base filepath line)
     (when-let* ((line-info (pr-review--get-diff-line-info (point))))
       (setq head-or-base (if (equal (car line-info) "LEFT") 'base 'head)
             filepath (cadr line-info)
             line (cddr line-info)))
     (when (or current-prefix-arg (null head-or-base) (null filepath))
       (let ((res (completing-read "Ref: " '("head" "base") nil t)))
         (setq head-or-base (intern res)))
       (setq filepath (read-from-minibuffer "File path: " filepath)))
     (list head-or-base filepath line)))
  (when (and head-or-base filepath)
    (let* ((content (pr-review--fetch-file filepath (pcase head-or-base
                                                      ('head (pr-review--current-commit-head))
                                                      ('base (pr-review--current-commit-base)))))
           (tempfile (pr-review--make-temp-file head-or-base filepath content)))
      (with-current-buffer (find-file-other-window tempfile)
        (goto-char (point-min))
        (when line
          (forward-line (1- line)))))))

(defun pr-review-ediff-file (filepath)
  "View the diff using `ediff'.
By default, view the file under current point (must in some diff).
When invoked with prefix, prompt for filepath."
  (interactive
   (let (filepath)
     (when-let* ((line-info (pr-review--get-diff-line-info (point))))
       (setq filepath (cadr line-info)))
     (when (or current-prefix-arg (null filepath))
       (setq filepath (completing-read "File:" (pr-review--find-all-file-names) nil 'require-match)))
     (list filepath)))
  (let* ((base-content (pr-review--fetch-file filepath (pr-review--current-commit-base)))
         (head-content (pr-review--fetch-file filepath (pr-review--current-commit-head))))
    (ediff-files (pr-review--make-temp-file 'base filepath base-content)
                 (pr-review--make-temp-file 'head filepath head-content))))

(defun pr-review-open-in-external-browser ()
  "Open current PR in external browser."
  (interactive)
  (browse-url-with-browser-kind
   'external
   (or (alist-get 'url pr-review--pr-info)
       (alist-get 'webUrl pr-review--pr-info))))

(define-obsolete-function-alias 'pr-review-open-in-default-browser
  'pr-review-open-in-external-browser "2026-02-09"
  "`pr-review-open-in-default-browser' is obsolete; use `pr-review-open-in-external-browser' instead.")

;; general dispatching functions, call other functions based on current context

(defun pr-review--review-thread-context-p (section)
  "Check whether SECTION is a review thread (or its children)."
  (or (pr-review--review-thread-section-p section)
      (pr-review--review-thread-item-section-p section)))

(defun pr-review--review-thread-context-and-resolvable-p (section)
  "Check weather SECTION is a review thread (or its children) and is resolvable."
  (when (pr-review--review-thread-item-section-p section)
    (setq section (oref section parent)))
  (and (pr-review--review-thread-section-p section)
       (oref section is-resolvable)))

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
    ((pred pr-review--review-thread-context-and-resolvable-p)
     (pr-review-resolve-thread))
    ;; in diff, or has pending review threads
    ((or (pred pr-review--diff-context-p)
         (pred (lambda (_) pr-review--pending-review-threads)))
     (call-interactively #'pr-review-submit-review))
    (_
     (funcall #'pr-review-general-interactive-action))))


(defun pr-review-context-edit ()
  "Edit on current point.
Based on current context, may be:
edit description, edit review comment, edit comment, edit pending diff review."
  (interactive)
  (pcase (magit-current-section)
    ((pred pr-review--description-section-p)
     (pr-review-edit-pr-description))
    ;; pr-review--review-thread-context-p matches both review-thread-section and review-thread-item-section.
    ;; where review-thread-section is only for gitlab.
    ;; all gitlab "notes" are either review-thread-item or review-thread; no comment-section or review-section
    ((pred pr-review--review-thread-context-p)
     (pr-review-edit-thread-item))
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

(defun pr-review--find-all-file-names ()
  "Return all file names in current buffer."
  (mapcar (lambda (section) (oref section value))
          (pr-review--find-all-file-sections magit-root-section)))

(defun pr-review-goto-file (filepath)
  "Goto section for FILEPATH in current buffer.
When called interactively, user can select filepath from list."
  (interactive (list (completing-read
                      "Goto file:"
                      (pr-review--find-all-file-names)
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
    (let* ((assignable-users (pr-review--get-assignable-users))
           (completion-extra-properties
            (list :annotation-function
                  (lambda (login)
                    (concat " " (alist-get 'name (gethash login assignable-users)))))))
      (completing-read-multiple
       "Request review: "
       (hash-table-keys assignable-users)
       nil 'require-match
       (string-join
        (mapcar (lambda (reviewer) (let-alist reviewer .requestedReviewer.login))
                (let-alist pr-review--pr-info .reviewRequests.nodes))
        ",")))))
  (let* ((assignable-users (pr-review--get-assignable-users))
         (ids (mapcar (lambda (login)
                        (let ((usr (gethash login assignable-users)))
                          (unless usr
                            (error "User %s not found" login))
                          (alist-get 'id usr)))
                      reviewer-logins)))
    (pr-review--post-request-reviews (alist-get 'id pr-review--pr-info) ids)
    (pr-review-refresh)))

(defun pr-review-set-labels (label-names)
  "Set labels for current PR, with a list of label names LABEL-NAMES.
This will override all existing labels (will clear all labels on empty).
When called interactively, user can select labels from list."
  (interactive
   (list
    (let* ((repo-labels (pr-review--get-repo-labels))
           (completion-extra-properties
            (list :annotation-function
                  (lambda (name)
                    (concat " " (alist-get 'description (gethash name repo-labels)))))))
      (completing-read-multiple
       "Labels: "
       (hash-table-keys repo-labels)
       nil 'require-match
       (string-join
        (mapcar (lambda (label-node) (alist-get 'name label-node))
                (let-alist pr-review--pr-info .labels.nodes))
        ",")))))
  (let* ((repo-labels (pr-review--get-repo-labels))
         (label-node-ids (mapcar (lambda (name)
                                   (let ((label (gethash name repo-labels)))
                                     (unless label
                                       (error "Label %s not found" name))
                                     (alist-get 'node_id label)))
                                 label-names))
         (pr-node-id (alist-get 'id pr-review--pr-info)))
    (pr-review--clear-labels pr-node-id)
    (when label-node-ids
      (pr-review--add-labels pr-node-id label-node-ids))
    (pr-review-refresh)))

(defun pr-review-update-subscription (state)
  "Update subscription to STATE for current PR.
Valid state (string): IGNORED, SUBSCRIBED, UNSUBSCRIBED."
  (interactive (list (completing-read "Update subscription: "
                                      pr-review--subscription-states
                                      nil 'require-match)))
  (when (member state pr-review--subscription-states)
    (pr-review--post-subscription-update (alist-get 'id pr-review--pr-info) state)
    (pr-review-refresh)))

(defun pr-review-goto-database-id (database-id)
  "Goto section with DATABASE-ID, which is used as the anchor in github urls.
Return t if found, nil otherwise."
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
      t)))

;; short helper for next function
(defun pr-review--make-abbrev-oid-to-commit-nodes (commit-nodes)
  (let* ((abbrev-oid-to-val (make-hash-table :test 'equal)))
    (dolist (n commit-nodes)
      (let-alist n
        (puthash .commit.abbreviatedOid n abbrev-oid-to-val)))
    abbrev-oid-to-val))

(defun pr-review-select-commit (&optional initial-input)
  "Interactively select some commits for review, with INITIAL-INPUT."
  (interactive)
  (let* ((commit-nodes (let-alist pr-review--pr-info .commits.nodes))
         (abbrev-oid-to-val (pr-review--make-abbrev-oid-to-commit-nodes commit-nodes))
         (completion-extra-properties
          (list :annotation-function (lambda (s) (let-alist (gethash s abbrev-oid-to-val)
                                                   (concat " " .commit.messageHeadline)))))
         (abbrev-oids
          (completing-read-multiple
           "Select commit (select two for a range, empty to reset): "
           (mapcar (lambda (n) (let-alist n .commit.abbreviatedOid)) commit-nodes)
           nil t initial-input))
         (indices (mapcar (lambda (x) (seq-position
                                       commit-nodes x (lambda (n xx) (equal (let-alist n .commit.abbreviatedOid) xx))))
                          abbrev-oids)))
    (when (seq-contains-p indices nil)
      (user-error "Invalid commit abbrev-oids"))
    (setq indices (sort (seq-uniq indices)))

    (setq pr-review--selected-commits nil
          pr-review--selected-commit-base nil
          pr-review--selected-commit-head nil)
    ;; if (null indices), keep above vars nil
    (when indices
      (unless (length< indices 3)
        (user-error "Must input 1 commit (to select only the commit) or 2 commits (to select a commit range)"))
      (setq pr-review--selected-commits
            (mapcar (lambda (i) (let-alist (nth i commit-nodes) .commit.oid))
                    (number-sequence (car indices) (car (last indices))))
            pr-review--selected-commit-head
            (car (last pr-review--selected-commits))
            pr-review--selected-commit-base
            (if (= (car indices) 0)
                (pr-review--current-commit-base)
              (let-alist (nth (- (car indices) 1) commit-nodes)
                .commit.oid)))))
  (pr-review-refresh))

(defun pr-review-update-reactions ()
  "Interactively select reactions for comment or description under point."
  (interactive)
  (let* ((section (magit-current-section))
         (all-reaction-names (mapcar (lambda (item) (car item)) pr-review-reaction-emojis))
         (completion-extra-properties
          (list :annotation-function
                (lambda (n) (concat " " (alist-get n pr-review-reaction-emojis "" nil 'equal)))))
         subject-id current-reaction-groups current-my-reactions)
    (if (or (pr-review--description-section-p section)
            (pr-review--review-section-p section)
            (pr-review--comment-section-p section)
            (pr-review--review-thread-item-section-p section))
        (setq subject-id (oref section value)
              current-reaction-groups (oref section reaction-groups))
      (user-error "Current point is not reactable"))
    (dolist (x current-reaction-groups)
      (when (alist-get 'viewerHasReacted x)
        (push (alist-get 'content x) current-my-reactions)))

    (pr-review--update-reactions
     subject-id
     (completing-read-multiple "Reactions: " all-reaction-names nil t
                               (concat (string-join current-my-reactions ",") ",")))
    (pr-review-refresh)))

(provide 'pr-review-action)
;;; pr-review-action.el ends here
