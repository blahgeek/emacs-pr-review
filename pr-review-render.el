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
(require 'pr-review-action)
(require 'magit-section)
(require 'magit-diff)
(require 'markdown-mode)
(require 'shr)

(defvar-local pr-review--diff-begin-point 0)
(defvar-local pr-review--char-pixel-width 0)

(defcustom pr-review-section-indent-width 2
  "Indent width for nested sections."
  :type 'integer
  :group 'pr-review)

(defun pr-review--format-timestamp (str)
  "Convert and format timestamp STR from json."
  (format-time-string "%b %d, %Y, %H:%M" (date-to-time str)))

(defun pr-review--propertize-username (username)
  (propertize (concat "@" username) 'face 'pr-review-author-face))

(defun pr-review--propertize-keyword (str)
  (propertize str 'face
              (cond
               ((member str '("MERGED" "SUCCESS" "COMPLETED" "APPROVED" "REJECTED"))
                'pr-review-success-state-face)
               ((member str '("FAILURE" "TIMED_OUT" "ERROR" "CHANGES_REQUESTED" "CLOSED" "CONFLICTING" "UNKNOWN"))
                'pr-review-error-state-face)
               ((member str '("RESOLVED" "OUTDATED"))
                'pr-review-info-state-face)
               (t
                'pr-review-state-face))))

(defun pr-review--insert-link (title url)
  (insert-button title 'face 'pr-review-link-face
                 'action (lambda (_) (browse-url url))))

(defun pr-review--dom-string (dom)
  (mapconcat (lambda (sub)
               (if (stringp sub)
                   sub
                 (pr-review--dom-string sub)))
             (dom-children dom)))

(defun pr-review--shr-tag-div (dom)
  "Function for rendering div tag in shr, special handle for suggested-changes."
  (if (not (string-match-p ".*suggested-changes.*" (or (dom-attr dom 'class) "")))
      (shr-tag-div dom)
    (let ((tbody (dom-by-tag dom 'tbody)))
      (let ((shr-current-font 'pr-review-info-state-face))
        (shr-insert "* Suggested change:")
        (insert "\n"))
      (dolist (tr (dom-non-text-children tbody))
        (dolist (td (dom-non-text-children tr))
          (let ((classes (split-string (or (dom-attr td 'class) ""))))
            (cond
             ((member "blob-num" classes) t)
             ((member "blob-code-deletion" classes)
              (let ((shr-current-font 'diff-indicator-removed))
                (shr-insert "-"))
              (insert (propertize (concat (pr-review--dom-string td)
                                          "\n")
                                  'face 'diff-removed)))
             ((member "blob-code-addition" classes)
              (let ((shr-current-font 'diff-indicator-added))
                (shr-insert "+"))
              (insert (propertize (concat (pr-review--dom-string td)
                                          "\n")
                                  'face 'diff-added)))
             (t
              (shr-generic td)))))))))

(defun pr-review--insert-html (body &optional indent)
  (let ((shr-indentation (* (or indent 0) pr-review--char-pixel-width))
        (shr-external-rendering-functions '((div . pr-review--shr-tag-div)))
        (start (point))
        dom)
    (with-temp-buffer
      (insert body)
      (setq dom (libxml-parse-html-region (point-min) (point-max))))
    ;; narrow the buffer and insert dom. otherwise there would be an extra new line at start
    (save-restriction
      (insert " ")
      (narrow-to-region start (1+ start))
      (goto-char start)
      (shr-insert-document dom)
      ;; delete the inserted " "
      (delete-char 1))
    (when (> shr-indentation 0)
      ;; shr-indentation does not work for images and code block
      ;; let's fix it: prepend space for any lines that does not starts with a space
      ;; (but we still need to use shr-indentation because otherwise the line will be too long)
      (save-excursion
        (goto-char start)
        (while (not (eobp))
          (unless (or (looking-at-p "\n")
                      (eq 'space (car-safe (get-text-property (point) 'display))))
            (beginning-of-line)
            (insert (propertize " " 'display `(space :width (,shr-indentation)))))
          (forward-line))))))

(defun pr-review--fontify (body lang-mode &optional margin)
  (with-current-buffer
      (get-buffer-create (format " *pr-review-fontification:%s*" lang-mode))
    (let ((inhibit-modification-hooks nil)
          (diff-font-lock-syntax 'hunk-also))
      (erase-buffer)
      (insert "\n"  ;; insert a newline at first line (and ignore later)
                    ;; to workaround markdown metadata syntax: https://github.com/jrblevin/markdown-mode/issues/328
              (string-replace "\r\n" "\n" body)
              " \n")
      (unless (eq major-mode lang-mode)
        (funcall lang-mode))
      (condition-case-unless-debug nil
          (font-lock-ensure)
        (error nil)))

    ;; delete invisible texts
    (let (match)
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'invisible))
        (let ((beg (prop-match-beginning match))
              (end (prop-match-end match)))
          (remove-text-properties beg end '(invisible nil)))))

    (when (eq lang-mode 'diff-mode)
      (save-excursion
        (dolist (ol (overlays-in (point-min) (point-max)))
          (when (eq (overlay-get ol 'diff-mode) 'syntax)
            (when-let ((face (overlay-get ol 'face)))
              (add-face-text-property (overlay-start ol)
                                      (overlay-end ol)
                                      face))))
        (goto-char (point-min))
        (remove-overlays (point-min) (point-max) 'diff-mode 'syntax)))

    (let ((res (buffer-substring 2 (point-max))))  ;; start at 2: skip first newline
      (when margin
        (setq res (replace-regexp-in-string (rx bol) (make-string margin ?\s) res)))
      res)))


(defun pr-review--insert-fontified (body lang-mode &optional margin)
  (insert (pr-review--fontify body lang-mode margin)))


(defun pr-review--insert-diff (diff)
  (let ((beg (point)))
    (setq-local pr-review--diff-begin-point beg)

    (if (not diff)
        (insert (propertize "Diff not available\n" 'face 'pr-review-error-state-face))
      (pr-review--insert-fontified diff 'diff-mode)
      (goto-char beg)
      (magit-wash-sequence (apply-partially 'magit-diff-wash-diff '())))

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
    (goto-char (point-min))
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
  "Goto diff line for FILEPATH, DIFFSIDE (string, left or right) and LINE,
return t on success."
  (goto-char pr-review--diff-begin-point)
  (when-let ((match (text-property-search-forward
                     (if (equal diffside "LEFT")
                         'pr-review-diff-line-left
                       'pr-review-diff-line-right)
                     (cons filepath line)
                     (lambda (target val)  ;; line may be null, in which case, match any line
                       (and (equal (car target) (car val))
                            (or (null (cdr target))
                                (equal (cdr target) (cdr val))))))))
    (goto-char (prop-match-beginning match))
    t))

(defun pr-review--insert-in-diff-pending-review-thread (pending-review-thread &optional allow-fallback)
  "If ALLOW-FALLBACK is non-nil, when the line for the thread cannot be found,
it will be inserted at the beginning."
  (save-excursion
    (let (beg end)
      (let-alist pending-review-thread
        (when (or (pr-review--goto-diff-line .path .side .line)
                  allow-fallback)
          (forward-line)
          (setq beg (point))
          (insert (propertize (concat "> PENDING comment for "
                                      (if .startLine
                                          (format "%s:%s to %s:%s" .startSide .startLine .side .line)
                                        (format "%s:%s" .side .line))
                                      "\n")
                              'face 'pr-review-in-diff-pending-begin-face))
          (pr-review--insert-fontified .body 'gfm-mode)
          (insert (propertize " \n" 'face 'pr-review-in-diff-pending-end-face))
          (setq end (point))))
      (when beg
        (add-text-properties
         beg end
         (list 'pr-review-pending-review-thread pending-review-thread))))))

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
            'face 'pr-review-in-diff-thread-title-face
            'pr-review-eldoc-content (let-alist (car .comments.nodes)
                                       (concat (pr-review--propertize-username .author.login)
                                               ": " .body))))
          (insert-button
           "Go to thread"
           'face 'pr-review-button-face
           'action (lambda (_)
                     (push-mark)
                     (pr-review--goto-section-with-value .id)))
          (insert (propertize "\n" 'face 'pr-review-in-diff-thread-title-face)))))))

(defun pr-review--insert-review-thread-section (top-comment review-thread)
  (magit-insert-section section (pr-review--review-thread-section
                                 (alist-get 'id review-thread)
                                 (eq t (alist-get 'isCollapsed review-thread)))
    (oset section top-comment-id (alist-get 'id top-comment))
    (oset section is-resolved (eq t (alist-get 'isResolved review-thread)))
    (let-alist review-thread
      (magit-insert-heading
        (make-string pr-review-section-indent-width ?\s)
        (propertize
         (concat
          .path (when .line (if .startLine
                                (format ":%s-%s" .startLine .line)
                              (format ":%s" .line))))
         'face 'magit-section-secondary-heading)
        (when (eq t .isResolved)
          (concat " - " (propertize "RESOLVED" 'face 'pr-review-info-state-face)))
        (when (eq t .isOutdated)
          (concat " - " (propertize "OUTDATED" 'face 'pr-review-info-state-face)))))
    (insert
     (make-string pr-review-section-indent-width ?\s)
     (propertize " \n" 'face 'pr-review-thread-diff-begin-face))
    (let ((diffhunk-lines (split-string (alist-get 'diffHunk top-comment) "\n"))
          beg end)
      (setq beg (point))
      (while (length> diffhunk-lines 4)   ;; diffHunk may be very long, only keep last 4 lines
        (setq diffhunk-lines (cdr diffhunk-lines)))
      (pr-review--insert-fontified (string-join diffhunk-lines "\n") 'diff-mode
                                   pr-review-section-indent-width)
      (setq end (point))
      (make-button beg end
                   'face nil
                   'help-echo "Click to go to the line in diff."
                   'action (lambda (_)
                             (push-mark)
                             (let-alist review-thread
                               (pr-review--goto-diff-line .path .diffSide .line)))))
    (insert (propertize " \n" 'face 'pr-review-thread-diff-end-face))
    (mapc (lambda (cmt)
            (let-alist cmt
              (magit-insert-section item-section (pr-review--review-thread-item-section .id)
                (oset item-section updatable .viewerCanUpdate)
                (oset item-section body .body)
                (magit-insert-heading
                  (make-string (* 2 pr-review-section-indent-width) ?\s)
                  (pr-review--propertize-username .author.login)
                  " - "
                  (propertize (pr-review--format-timestamp .createdAt)
                              'face 'pr-review-timestamp-face))
                (pr-review--insert-html .bodyHTML (* 2 pr-review-section-indent-width))
                (insert "\n"))))
          (let-alist review-thread .comments.nodes))

    (insert (make-string (* 2 pr-review-section-indent-width) ?\s))
    (insert-button "Reply to thread"
                   'face 'pr-review-button-face
                   'action 'pr-review-reply-to-thread)
    (insert "  ")
    (let ((resolved (eq t (alist-get 'isResolved review-thread))))
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
      (when (or top-comment-and-review-thread-list
                (not (equal .state "COMMENTED"))
                (not (string-empty-p .body)))
        (magit-insert-section section (pr-review--review-section .id)
          (oset section updatable .viewerCanUpdate)
          (oset section body .body)
          (magit-insert-heading
            (propertize "Reviewed by " 'face 'magit-section-heading)
            (pr-review--propertize-username .author.login)
            " - "
            (pr-review--propertize-keyword .state)
            " - "
            (propertize (pr-review--format-timestamp .createdAt) 'face 'pr-review-timestamp-face))
          (unless (string-empty-p .body)
            (pr-review--insert-html .bodyHTML))
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
        (propertize "Commented by " 'face 'magit-section-heading)
        (pr-review--propertize-username .author.login)
        " - "
        (propertize (pr-review--format-timestamp .createdAt) 'face 'pr-review-timestamp-face))
      (pr-review--insert-html .bodyHTML)
      (insert "\n"))))

(defun pr-review--insert-event-section (event)
  (let-alist event
    (magit-insert-section (pr-review--event-section .id 'hide)
      (magit-insert-heading
        (propertize "* " 'face 'magit-section-heading)
        (apply #'concat
               (pcase .__typename
                 ("AssignedEvent"
                  (list
                   (propertize "Assigned to " 'face 'magit-section-heading)
                   (pr-review--propertize-username (or .assignee.login ""))
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)))
                 ("ReviewRequestedEvent"
                  (list
                   (propertize "Requested review from " 'face 'magit-section-heading)
                   (pr-review--propertize-username (or .requestedReviewer.login ""))
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)))
                 ("ReviewRequestRemovedEvent"
                  (list
                   (propertize "Removed review request from " 'face 'magit-section-heading)
                   (pr-review--propertize-username (or .requestedReviewer.login ""))
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)))
                 ("MergedEvent"
                  (list
                   (propertize "Merged" 'face 'pr-review-success-state-face)
                   (propertize " into " 'face 'magit-section-heading)
                   (propertize .mergeRefName 'face 'pr-review-branch-face)
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)))
                 ("ClosedEvent"
                  (list
                   (propertize "Closed" 'face 'pr-review-error-state-face)
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)))
                 ("PullRequestCommit"
                  (list
                   (propertize "Pushed commit" 'face 'magit-section-heading)
                   " - "
                   (propertize .commit.abbreviatedOid 'face 'pr-review-hash-face)
                   " - "
                   (propertize (pr-review--format-timestamp .commit.pushedDate)
                               'face 'pr-review-timestamp-face)
                   " - "
                   .commit.messageHeadline))
                 ("HeadRefForcePushedEvent"
                  (list
                   (propertize "Force pushed" 'face 'magit-section-heading)
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)
                   " - "
                   (propertize .beforeCommit.abbreviatedOid 'face 'pr-review-hash-face)
                   " -> "
                   (propertize .afterCommit.abbreviatedOid 'face 'pr-review-hash-face)))))
        (when .createdAt
          (concat
           " - "
           (propertize (pr-review--format-timestamp .createdAt) 'face 'pr-review-timestamp-face)
           ))))
    (insert "\n")))

(defun pr-review--insert-reviewers-info (pr-info)
  (let ((groups (make-hash-table :test 'equal)))
    (let ((review-requests (let-alist pr-info .reviewRequests.nodes)))
      (puthash "PENDING" (mapcar (lambda (review-request)
                                   (let-alist review-request .requestedReviewer.login))
                                 review-requests)
               groups))
    (mapc (lambda (opinionated-review)
            (let-alist opinionated-review
              (push .author.login (gethash .state groups))))
          (let-alist pr-info .latestOpinionatedReviews.nodes))
    (maphash (lambda (status users)
               (insert (pr-review--propertize-keyword status)
                       ": "
                       (mapconcat #'pr-review--propertize-username users ", "))
               (when (equal status "PENDING")
                 (insert " ")
                 (insert-button
                  "Request Review"
                  'face 'pr-review-button-face
                  'action (lambda (_) (call-interactively 'pr-review-request-reviews))))
               (insert "\n"))
             groups)))

(defun pr-review--insert-assignees-info (pr-info)
  (insert (pr-review--propertize-keyword "ASSIGNED")
          ": "
          (mapconcat (lambda (assignee) (pr-review--propertize-username (alist-get 'login assignee)))
                     (let-alist pr-info .assignees.nodes) ", ")
          "\n"))

(defun pr-review--insert-check-section (status-check-rollup required-contexts)
  (magit-insert-section (pr-review--check-section 'check-section-id)
    (magit-insert-heading (concat (propertize "Check status - " 'face 'magit-section-heading)
                                  (pr-review--propertize-keyword
                                   (or (alist-get 'state status-check-rollup) "UNKNOWN"))))
    (let ((valid-contexts (mapcar (lambda (node) (alist-get 'context node))
                                  (let-alist status-check-rollup .contexts.nodes))))
      (mapc (lambda (required-context)
              (unless (member required-context valid-contexts)
                (insert (concat "- "
                                (propertize required-context 'face 'pr-review-author-face)
                                ": "
                                (propertize "REQUIRED" 'face 'pr-review-error-state-face)
                                "\n"))))
            required-contexts))
    (mapc (lambda (node)
            (let-alist node
              (pcase .__typename
                ("CheckRun"
                 (insert (concat "- "
                                 (propertize .name 'face 'pr-review-author-face)
                                 ": "
                                 (pr-review--propertize-keyword .status)
                                 (when .conclusion
                                   (concat " - "
                                           (pr-review--propertize-keyword .conclusion)))
                                 (when .title
                                   (concat " - " .title))
                                 "\n")))
                ("StatusContext"
                 (insert (concat "- "
                                 (propertize .context 'face 'pr-review-author-face)
                                 ": "
                                 (pr-review--propertize-keyword .state)
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

(defun pr-review--get-label-foreground (background-hex)
  (when (string-match (rx bol
                          (group (= 2 (any xdigit)))
                          (group (= 2 (any xdigit)))
                          (group (= 2 (any xdigit)))
                          eol)
                      background-hex)
    (let* ((r (string-to-number (match-string 1 background-hex) 16))
           (g (string-to-number (match-string 2 background-hex) 16))
           (b (string-to-number (match-string 3 background-hex) 16)))
      (if (> (+ r g b) 384)
          "#000000"
        "#ffffff"))))

(defun pr-review--insert-pr-body (pr diff)
  (let ((top-comment-id-to-review-thread
         (pr-review--build-top-comment-id-to-review-thread-map pr))
        (timeline-items
          (append
           (mapcar (lambda (x) (cons x 'review)) (let-alist pr .reviews.nodes))
           (mapcar (lambda (x) (cons x 'comment)) (let-alist pr .comments.nodes))
           (mapcar (lambda (x) (cons x 'event)) (let-alist pr .timelineItems.nodes)))))
    (let ((sort-key-fn (lambda (item) (let-alist (car item)
                                        (or .createdAt .commit.pushedDate "")))))
      (setq timeline-items (sort timeline-items (lambda (a b)
                                                  (string< (funcall sort-key-fn a)
                                                           (funcall sort-key-fn b))))))
    (let-alist pr
      (pr-review--insert-link .url .url)
      (insert "\n"
              (propertize .baseRefName 'face 'pr-review-branch-face)
              " <- "
              (propertize .headRefName 'face 'pr-review-branch-face))
      (when .labels.nodes
        (insert
         "  "
         (mapconcat (lambda (label)
                      (propertize (alist-get 'name label)
                                  'face
                                  `(:background ,(concat "#" (alist-get 'color label))
                                                :foreground ,(pr-review--get-label-foreground (alist-get 'color label))
                                                :inherit pr-review-label-face)))
                    .labels.nodes " ")))
      (insert "\n")
      (insert (pr-review--propertize-keyword .state)
              (if (equal .state "OPEN")
                  (concat " - " (pr-review--propertize-keyword .mergeable))
                "")
              " - "
              (propertize (concat "@" .author.login) 'face 'pr-review-author-face)
              " - "
              (propertize (pr-review--format-timestamp .createdAt) 'face 'pr-review-timestamp-face)
              "\n\n")
      (pr-review--insert-reviewers-info pr)
      (pr-review--insert-assignees-info pr)
      (insert "\n")
      (magit-insert-section section (pr-review--description-section)
        (oset section body .body)
        (oset section updatable .viewerCanUpdate)
        (magit-insert-heading "Description")
        (pr-review--insert-html .bodyHTML))
      (insert "\n"))
    (dolist (timeline-item timeline-items)
      (pcase (cdr timeline-item)
        ('review
         (pr-review--insert-review-section (car timeline-item) top-comment-id-to-review-thread))
        ('comment
         (pr-review--insert-comment-section (car timeline-item)))
        ('event
         (pr-review--insert-event-section (car timeline-item)))))
    (insert "\n")
    (let ((status-check-rollup (let-alist
                                   (nth 0 (let-alist pr .commits.nodes))
                                 .commit.statusCheckRollup))
          (required-contexts (let-alist pr .baseRef.refUpdateRule.requiredStatusCheckContexts)))
      (when (or status-check-rollup required-contexts)
        (pr-review--insert-check-section status-check-rollup required-contexts)
        (insert "\n")))
    (magit-insert-section (pr-review--diff-section)
      (magit-insert-heading
        (let-alist pr
          (format "Files changed (%s files; %s additions, %s deleletions)"
                  (length .files.nodes)
                  (apply '+ (mapcar (lambda (x) (alist-get 'additions x)) .files.nodes))
                  (apply '+ (mapcar (lambda (x) (alist-get 'deletions x)) .files.nodes)))))
      (pr-review--insert-diff diff)
      (insert "\n"
              "Review Changes with:")
      (dolist (event '("COMMENT" "APPROVE" "REQUEST_CHANGES"))
        (insert " ")
        (insert-button event 'face 'pr-review-button-face
                       'action (lambda (_) (pr-review-submit-review event))))
      (insert ", or ")
      (insert-button "COMMENT ONLY" 'face 'pr-review-button-face
                     'action (lambda (_) (pr-review-comment)))
      (insert "\n"))
    (mapc 'pr-review--insert-in-diff-review-thread-link
          (let-alist pr .reviewThreads.nodes))))

(defun pr-review--insert-pr (pr diff)
  (setq pr-review--char-pixel-width (shr-string-pixel-width "-"))
  (magit-insert-section section (pr-review--root-section)
    (let-alist pr
      (oset section title .title)
      (oset section updatable .viewerCanUpdate)
      (magit-insert-heading
        (propertize (alist-get 'title pr)'face 'pr-review-title-face)))
    (insert "\n")
    (pr-review--insert-pr-body pr diff)))


(provide 'pr-review-render)
;;; pr-review-render.el ends here
