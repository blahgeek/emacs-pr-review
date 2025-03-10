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
(defvar pr-review--last-read-time nil)  ;; dynamically bound. only assigned temporarily. see pr-review-open

(defcustom pr-review-section-indent-width 2
  "Indent width for nested sections."
  :type 'integer
  :group 'pr-review)

(defcustom pr-review-fringe-icons t
  "Display icons in the fringe indicating PR-review comments."
  :type 'boolean
  :group 'pr-review)

(defun pr-review--format-timestamp (str)
  "Convert and format timestamp STR from json."
  (concat
   (propertize (format-time-string "%b %d, %Y, %H:%M" (date-to-time str))
               'face 'pr-review-timestamp-face)
   (when (and pr-review--last-read-time (string> str pr-review--last-read-time))
     (concat " " (propertize "UNREAD"
                             'face 'pr-review-state-face
                             'pr-review-unread t)))))

(defun pr-review--propertize-username (username)
  "Format and propertize USERNAME."
  (propertize (concat "@" username) 'face 'pr-review-author-face))

(defun pr-review--propertize-keyword (str)
  "Propertize keyword STR with optional face."
  (let* ((trim-pattern "[][ \t\n\r]+")
         (trimmed-str (string-trim str trim-pattern trim-pattern)))
    (propertize str 'face
                (cond
                 ((member trimmed-str '("MERGED" "SUCCESS" "COMPLETED" "APPROVED" "REJECTED"))
                  'pr-review-success-state-face)
                 ((member trimmed-str '("FAILURE" "TIMED_OUT" "ERROR" "CHANGES_REQUESTED" "CLOSED" "CONFLICTING" "UNKNOWN"))
                  'pr-review-error-state-face)
                 ((member trimmed-str '("RESOLVED" "OUTDATED" "WARNING"))
                  'pr-review-info-state-face)
                 (t
                  'pr-review-state-face)))))

(defun pr-review--insert-link (title url)
  "Insert a link URL with TITLE."
  (insert-button title 'face 'pr-review-link-face
                 'action (lambda (_) (browse-url url))))

(defun pr-review--make-link (title url)
  "Make a link (as string) of URL with TITLE."
  (button-buttonize (propertize title 'face 'pr-review-link-face)
                    (lambda (_) (browse-url url))))

(defun pr-review--dom-string (dom)
  "Return string of DOM."
  (mapconcat (lambda (sub)
               (if (stringp sub)
                   sub
                 (pr-review--dom-string sub)))
             (dom-children dom) ""))

(defun pr-review--shr-tag-div (dom)
  "Rendering div tag as DOM in shr, with special handle for suggested-changes."
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

(defun pr-review--insert-html (body &optional indent extra-face)
  "Insert html content BODY.
INDENT is an optional number, if provided,
INDENT count of spaces are added at the start of every line.
If EXTRA-FACE is given, it is added to the inserted text
in addition to other faces."
  (let ((shr-indentation (* (or indent 0) pr-review--char-pixel-width))
        (shr-external-rendering-functions '((div . pr-review--shr-tag-div)))
        (start (point))
        end
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
      (delete-char 1)
      (setq end (point)))
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
          (forward-line))))
    ;; additional face for the inserted region
    (when extra-face
      (add-face-text-property start end extra-face))))

(defun pr-review--maybe-insert-reactions (reaction-group &optional indent)
  "Insert REACTION-GROUP if not nil.
INDENT is an optional number of extra spaces at the start of the line."
  (let (s)
    (dolist (group reaction-group)
      (let-alist group
        (when (and (> .reactors.totalCount 0) .content)
          (when s
            (setq s (concat s "  ")))
          (setq s (concat
                   s
                   (format "%s%d"
                           (alist-get .content pr-review-reaction-emojis .content nil 'equal)
                           .reactors.totalCount)
                   (when .viewerHasReacted
                     "#"))))))
    (when s
      (when indent
        (insert (propertize " " 'display `(space :width (,(* indent pr-review--char-pixel-width))))))
      (insert (propertize s 'face 'pr-review-reaction-face) "\n"))))

(defun pr-review--fontify (body lang-mode &optional margin)
  "Fontify content BODY as LANG-MODE, return propertized string.
MARGIN is an optional number, if provided,
MARGIN count of spaces are added at the start of every line."
  (with-current-buffer
      (get-buffer-create (format " *pr-review-fontification:%s*" lang-mode))
    (let ((inhibit-modification-hooks nil)
          (diff-font-lock-syntax pr-review-diff-font-lock-syntax))
      (erase-buffer)
      (insert "\n"  ;; insert a newline at first line (and ignore later)
                    ;; to workaround markdown metadata syntax: https://github.com/jrblevin/markdown-mode/issues/328
              (replace-regexp-in-string "\r\n" "\n" body nil t))
      (unless (string-suffix-p "\n" body)
        (insert "\n"))
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

(defun pr-review--insert-fontified (body lang-mode &optional margin extra-face)
  "Fontify BODY as LANG-MODE with MARGIN and insert it, see `pr-review--fontify'."
  (let ((start (point))
        end)
    (insert (pr-review--fontify body lang-mode margin))
    (setq end (point))
    (when extra-face
      (add-face-text-property start end extra-face))))

(defun pr-review--insert-diff (diff)
  "Insert pull request diff DIFF, wash it using magit."
  (let ((beg (point)))
    (setq-local pr-review--diff-begin-point beg)

    (if (not diff)
        (insert (propertize "Diff not available\n" 'face 'pr-review-error-state-face))
      (pr-review--insert-fontified diff 'diff-mode)
      (goto-char beg)
      (magit-wash-sequence (apply-partially #'magit-diff-wash-diff '())))

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
             `(pr-review-diff-line-right ,(cons filename (cdr current-left-right))))))))))

(defun pr-review--hide-generated-files ()
  "Hide file sections for generated files."
  (mapc (lambda (section)
          (when (string-match-p pr-review-generated-file-regexp (oref section value))
            (magit-section-hide section)))
        (pr-review--find-all-file-sections magit-root-section)))

(defun pr-review--find-section-with-value (value)
  "Find and return the magit section object matching VALUE."
  (save-excursion
    (goto-char (point-min))
    (when-let ((match (text-property-search-forward
                       'magit-section value
                       (lambda (target prop-value)
                         (and prop-value
                              (equal (oref prop-value value) target))))))
      (get-text-property (prop-match-beginning match) 'magit-section))))

(defun pr-review--goto-section-with-value (value)
  "Go to the magit section object matching VALUE."
  (when-let ((section (pr-review--find-section-with-value value)))
    (goto-char (oref section start))))

(defun pr-review--goto-diff-line (filepath diffside line)
  "Goto diff line for FILEPATH, DIFFSIDE (string, left or right) and LINE.
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


(define-fringe-bitmap 'pr-review-comment
  [#b0000000000000000
   #b0000000000000000
   #b0000000000000000
   #b0000000000000000
   #b0000000000000000
   #b0111111111111100
   #b1111111111111110
   #b1111111111111110
   #b1110000000011110
   #b1111111111111110
   #b1111111111111110
   #b1110000011111110
   #b1111111111111110
   #b1111111111111110
   #b1111111111111110
   #b0111111111111100
   #b0000000011111000
   #b0000000001111000
   #b0000000000111000
   #b0000000000011000]
  nil 16 'center)

(defun pr-review--insert-in-diff-pending-review-thread (pending-review-thread &optional allow-fallback)
  "Insert a pending review thread inside the diff for PENDING-REVIEW-THREAD.
If ALLOW-FALLBACK is non-nil, when the line for the thread cannot be found.
It will be inserted at the beginning."
  (save-excursion
    (let (beg end)
      (let-alist pending-review-thread
        (when (or (pr-review--goto-diff-line .path .side .line)
                  allow-fallback)
          (forward-line)
          (setq beg (point))
          (insert (apply #'propertize
                         (concat "> PENDING comment for "
                                 (if .startLine
                                     (format "%s:%s to %s:%s" .startSide .startLine .side .line)
                                   (format "%s:%s" .side .line))
                                 "\n")
                         'face 'pr-review-in-diff-pending-begin-face
                         (when pr-review-fringe-icons
                           (list 'line-prefix
                                 (propertize " " 'display '(left-fringe
                                                            pr-review-comment
                                                            pr-review-fringe-comment-pending))))))
          (pr-review--insert-fontified .body 'gfm-mode nil
                                       'pr-review-in-diff-pending-body-face)
          (insert (propertize " \n" 'face 'pr-review-in-diff-pending-end-face))
          (setq end (point))))
      (when beg
        (add-text-properties
         beg end
         (list 'pr-review-pending-review-thread pending-review-thread))))))

(defun pr-review--insert-in-diff-review-thread-link (review-thread)
  "Insert REVIEW-THREAD inside the diff section."
  (let-alist review-thread
    ;; when only some commits are selected, in-diff review threads are not displayed.
    ;; I don't have an easy way to know if certain review threads is for the selected commits.
    (when (and (not .isOutdated) (null pr-review--selected-commits))
      (save-excursion
        (when (pr-review--goto-diff-line
               .path .diffSide .line)
          (forward-line)
          (insert
           (apply
            #'propertize
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
                                               ": " .body))
            (when pr-review-fringe-icons
              (list 'line-prefix
                    (propertize " " 'display `(left-fringe pr-review-comment
                                                           (if .isResolved
                                                               pr-review-fringe-comment-resolved
                                                             pr-review-fringe-comment-open)))))))
          (insert-button
           "Go to thread"
           'face 'pr-review-button-face
           'action (lambda (_)
                     (push-mark)
                     (pr-review--goto-section-with-value .id)
                     (recenter)))
          (insert (propertize "\n" 'face 'pr-review-in-diff-thread-title-face)))))))

(defun pr-review--insert-in-diff-checkrun-annotation (annotation)
  "Insert CheckRun ANNOTATION inside the diff section."
  (let-alist annotation
    (save-excursion
      (when (pr-review--goto-diff-line .path "RIGHT" .location.end.line)
        (forward-line)
        (insert
         (concat
          (propertize "> " 'face 'pr-review-in-diff-thread-title-face)
          (pr-review--propertize-keyword (format "[%s]" .annotationLevel))
          (propertize (format " %s: %s" .title .message) 'face 'pr-review-in-diff-thread-title-face)
          "\n")
         )))))

(defun pr-review--insert-review-thread-section (top-comment review-thread)
  "Insert review thread section with TOP-COMMENT and REVIEW-THREAD."
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
         'face 'pr-review-thread-item-title-face)
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
      ;; diffHunk may be very long, only keep last N lines
      (while (> (length diffhunk-lines) pr-review-diff-hunk-limit)
        (setq diffhunk-lines (cdr diffhunk-lines)))
      (pr-review--insert-fontified (string-join diffhunk-lines "\n") 'diff-mode
                                   pr-review-section-indent-width
                                   'pr-review-thread-diff-body-face)
      (setq end (point))
      (make-button beg end
                   'face nil
                   'help-echo "Click to go to the line in diff."
                   'action (lambda (_)
                             (push-mark)
                             (let-alist review-thread
                               (pr-review--goto-diff-line .path .diffSide .line)
                               (recenter)))))
    (insert (propertize " \n" 'face 'pr-review-thread-diff-end-face))
    (mapc (lambda (cmt)
            (let-alist cmt
              (magit-insert-section item-section (pr-review--review-thread-item-section .id)
                (oset item-section databaseId .databaseId)
                (oset item-section updatable .viewerCanUpdate)
                (oset item-section body .body)
                (oset item-section reaction-groups .reactionGroups)
                (magit-insert-heading
                  (make-string (* 2 pr-review-section-indent-width) ?\s)
                  (pr-review--propertize-username .author.login)
                  " - "
                  (pr-review--format-timestamp .createdAt))
                (pr-review--insert-html .bodyHTML (* 2 pr-review-section-indent-width)
                                        'pr-review-thread-comment-face)
                (pr-review--maybe-insert-reactions .reactionGroups (* 2 pr-review-section-indent-width))
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
  "Insert review section for REVIEW and mapping TOP-COMMENT-ID-TO-REVIEW-THREAD."
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
          (oset section databaseId .databaseId)
          (oset section updatable .viewerCanUpdate)
          (oset section body .body)
          (oset section reaction-groups .reactionGroups)
          (magit-insert-heading
            (propertize "Reviewed by " 'face 'magit-section-heading)
            (pr-review--propertize-username .author.login)
            " - "
            (pr-review--propertize-keyword .state)
            " - "
            (pr-review--format-timestamp .createdAt))
          (unless (string-empty-p .body)
            (pr-review--insert-html .bodyHTML))
          (pr-review--maybe-insert-reactions .reactionGroups)
          (insert "\n")
          (dolist (top-comment-and-review-thread top-comment-and-review-thread-list)
            (apply #'pr-review--insert-review-thread-section top-comment-and-review-thread))
          (when top-comment-and-review-thread-list
            (insert "\n")))))))

(defun pr-review--insert-comment-section (cmt)
  "Insert comment section with comment CMT."
  (let-alist cmt
    (magit-insert-section section (pr-review--comment-section .id)
      (oset section databaseId .databaseId)
      (oset section updatable .viewerCanUpdate)
      (oset section body .body)
      (oset section reaction-groups .reactionGroups)
      (magit-insert-heading
        (propertize "Commented by " 'face 'magit-section-heading)
        (pr-review--propertize-username .author.login)
        " - "
        (pr-review--format-timestamp .createdAt))
      (pr-review--insert-html .bodyHTML)
      (pr-review--maybe-insert-reactions .reactionGroups)
      (insert "\n"))))

(defun pr-review--insert-misc-event-section (event)
  "Insert event section with EVENT."
  (let-alist event
    (magit-insert-section (pr-review--event-section .id 'hide)
      (magit-insert-heading
        (propertize "* " 'face 'magit-section-heading)
        (apply #'concat
               (pcase .__typename
                 ("AssignedEvent"
                  (list
                   (propertize "Assigned to " 'face 'magit-section-heading)
                   (mapconcat (lambda (item)
                                (let-alist item
                                  (pr-review--propertize-username .assignee.login)))
                              .groupedItems ", ")
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)))
                 ("UnassignedEvent"
                  (list
                   (propertize "Unassigned to " 'face 'magit-section-heading)
                   (mapconcat (lambda (item)
                                (let-alist item
                                  (pr-review--propertize-username .assignee.login)))
                              .groupedItems ", ")
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)))
                 ("ReviewRequestedEvent"
                  (list
                   (propertize "Requested review from " 'face 'magit-section-heading)
                   (mapconcat (lambda (item)
                                (let-alist item
                                  (pr-review--propertize-username .requestedReviewer.login)))
                              .groupedItems ", ")
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)))
                 ("ReviewRequestRemovedEvent"
                  (list
                   (propertize "Removed review request from " 'face 'magit-section-heading)
                   (mapconcat (lambda (item)
                                (let-alist item
                                  (pr-review--propertize-username .requestedReviewer.login)))
                              .groupedItems ", ")
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
                 ("ReopenedEvent"
                  (list
                   (propertize "Reopened" 'face 'pr-review-success-state-face)
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)))
                 ("PullRequestCommit"
                  (list
                   (propertize (format "Pushed %d commits" (length .groupedItems))
                               'face 'magit-section-heading)))
                 ("HeadRefForcePushedEvent"
                  (list
                   (propertize (format "Force pushed %d times" (length .groupedItems))
                               'face 'magit-section-heading)
                   (propertize " by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)
                   " - "
                   (propertize
                    (let-alist (car .groupedItems)
                      (or .beforeCommit.abbreviatedOid "?"))
                    'face 'pr-review-hash-face)
                   " -> "
                   (propertize
                    (let-alist (car (last .groupedItems))
                      (or .afterCommit.abbreviatedOid "?"))
                    'face 'pr-review-hash-face)))
                 ("CrossReferencedEvent"
                  (list
                   (propertize "Mentioned by " 'face 'magit-section-heading)
                   (pr-review--propertize-username .actor.login)
                   (propertize " in " 'face 'magit-section-heading)
                   (pr-review--make-link .source.title .source.url)))))
        (when .createdAt
          (concat
           " - "
           (pr-review--format-timestamp .createdAt))))
      ;; body
      (pcase .__typename
        ("PullRequestCommit"
         (dolist (commit .groupedItems)
           (let-alist commit
             (insert "- "
                     (propertize .commit.abbreviatedOid 'face 'pr-review-hash-face)
                     " "
                     .commit.messageHeadline
                     "\n"))))))
    (insert "\n")))

(defun pr-review--insert-reviewers-info (pr-info)
  "Insert reviewers info for PR-INFO."
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
                  'action (lambda (_) (call-interactively #'pr-review-request-reviews))))
               (insert "\n"))
             groups)))

(defun pr-review--insert-assignees-info (pr-info)
  "Insert assignees info for PR-INFO."
  (let-alist pr-info
    (when .assignees.nodes
      (insert (pr-review--propertize-keyword "ASSIGNED")
              ": "
              (mapconcat (lambda (assignee) (pr-review--propertize-username (alist-get 'login assignee)))
                         .assignees.nodes ", ")
              "\n"))))

(defun pr-review--insert-subscription-info (pr-info)
  "Insert subscription info and actions for PR-INFO."
  (let-alist pr-info
    (when .viewerCanSubscribe
      (insert-button .viewerSubscription
                     'face 'pr-review-button-face
                     'action (lambda (_) (call-interactively #'pr-review-update-subscription))))))

(defun pr-review--insert-commit-section (commits)
  "Insert commit section for a list of COMMITS."
  (magit-insert-section (pr-review--commit-section 'commit-section-id 'hide)
    (magit-insert-heading (format "Total %d commits" (length commits)))
    (dolist (commit commits)
      (let-alist commit
        (insert (if (or (null pr-review--selected-commits)
                        (member .commit.oid pr-review--selected-commits))
                    "* "
                  "- "))
        (insert-button .commit.abbreviatedOid
                       'face '(pr-review-hash-face pr-review-link-face)
                       'action (lambda (_) (pr-review-select-commit .commit.abbreviatedOid)))
        (insert " " .commit.messageHeadline "\n")))))

(defun pr-review--insert-check-section (status-check-rollup required-contexts)
  "Insert check section for STATUS-CHECK-ROLLUP and REQUIRED-CONTEXTS."
  (magit-insert-section (pr-review--check-section 'check-section-id)
    (magit-insert-heading (concat (propertize "Check status - " 'face 'magit-section-heading)
                                  (pr-review--propertize-keyword
                                   (or (alist-get 'state status-check-rollup) "UNKNOWN"))))
    ;; for REQUIRED: show "*" instead of "-"
    (let ((required-item-bullet-point (propertize "* " 'pr-review-eldoc-content "Required"))
          (valid-context-or-names (mapcar (lambda (node) (or (alist-get 'context node)
                                                             (alist-get 'name node)))
                                          (let-alist status-check-rollup .contexts.nodes))))
      (mapc (lambda (required-context)
              (unless (member required-context valid-context-or-names)
                (insert required-item-bullet-point
                        (propertize required-context 'face 'pr-review-check-face)
                        ": "
                        (propertize "EXPECTED" 'face 'pr-review-error-state-face)
                        "\n")))
            required-contexts)
      (mapc (lambda (node)
              (let-alist node
                (pcase .__typename
                  ("CheckRun"
                   (insert (concat (if (member .name required-contexts)
                                       required-item-bullet-point
                                     "- ")
                                   (propertize .name 'face 'pr-review-check-face)
                                   ": "
                                   (pr-review--propertize-keyword .status)
                                   (when .conclusion
                                     (concat " - "
                                             (pr-review--propertize-keyword .conclusion)))
                                   (when .title
                                     (concat " - " .title))
                                   "\n")))
                  ("StatusContext"
                   (insert (concat (if (member .context required-contexts)
                                       required-item-bullet-point
                                     "- ")
                                   (propertize .context 'face 'pr-review-check-face)
                                   ": "
                                   (pr-review--propertize-keyword .state)
                                   (when .description
                                     (concat " - " .description))
                                   " "))
                   (when .targetUrl
                     (pr-review--insert-link "Details" .targetUrl))
                   (insert "\n")))))
            (let-alist status-check-rollup .contexts.nodes)))
    (insert "\n")))

(defun pr-review--build-top-comment-id-to-review-thread-map (pr)
  "Build mapping top-comment-id -> review-thread for PR."
  (let ((res (make-hash-table :test 'equal)))
    (mapc (lambda (review-thread)
            (let* ((comments (let-alist review-thread .comments.nodes))
                   (top-comment (elt comments 0))
                   (top-comment-id (alist-get 'id top-comment)))
              (puthash top-comment-id review-thread res)))
          (let-alist pr .reviewThreads.nodes))
    res))

(defun pr-review--get-label-foreground (background-hex)
  "Get the foreground color hex for label based on its BACKGROUND-HEX."
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

(defun pr-review--insert-labels-info (pr-info)
  "Insert labels and action for PR-INFO."
  (let-alist pr-info
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
    (insert "  ")
    (insert-button
     "Edit Labels"
     'face 'pr-review-button-face
     'action (lambda (_) (call-interactively #'pr-review-set-labels)))))

(defun pr-review--insert-review-action-buttons ()
  "Insert text and buttons for review actions."
  (insert "Submit review with action:")
  (dolist (event pr-review--review-actions)
    (insert " ")
    (insert-button event 'face 'pr-review-button-face
                   'action (lambda (_) (pr-review-submit-review event))))
  (insert ", or ")
  (insert-button "COMMENT ONLY" 'face 'pr-review-button-face
                 'action (lambda (_) (pr-review-comment)))
  (insert "\n"))

(defun pr-review--insert-merge-close-reopen-action-buttons ()
  "Insert text and buttons for merge, close or reopen."
  (insert "Merge pull request with method:")
  (dolist (method pr-review--merge-methods)
    (insert " ")
    (insert-button method 'face 'pr-review-button-face
                   'action (lambda (_) (pr-review-merge method))))
  (when-let ((close-or-reopen-action (pr-review--close-or-reopen-action)))
    (insert ", or ")
    (insert-button (upcase (symbol-name close-or-reopen-action))
                   'face 'pr-review-button-face
                   'action (lambda (_) (pr-review-close-or-reopen))))
  (insert "\n"))

(defun pr-review--is-timeline-items-groupable (item-a item-b)
  "Check if timelineItems ITEM-A and ITEM-B should be grouped."
  (and (equal (alist-get '__typename item-a) (alist-get '__typename item-b))
       (pcase (alist-get '__typename item-a)
         ("PullRequestCommit" t)
         ((or "AssignedEvent"
              "UnassignedEvent"
              "HeadRefForcePushedEvent"
              "ReviewRequestedEvent"
              "ReviewRequestRemovedEvent")
          (and (equal (let-alist item-a .actor.login)
                      (let-alist item-b .actor.login))
               (> 300 (abs (- (float-time (date-to-time (let-alist item-a .createdAt)))
                              (float-time (date-to-time (let-alist item-b .createdAt)))))))))))

(defun pr-review--normalize-group-timeline-items (items)
  "Normalize and group .timelineItems.nodes ITEMS.
Some events can be merged into one item so that
it can be displayed in a single line."
  (let (groups current-group)
    (dolist (item items)
      ;; end current group if:
      ;; current group is not empty and it's not groupable with current item
      (when (and current-group
                 (not (pr-review--is-timeline-items-groupable (car current-group) item)))
        (push (nreverse current-group) groups)
        (setq current-group nil))
      (push item current-group))
    (when current-group
      (push (nreverse current-group) groups)
      (setq current-group nil))

    ;; process each group
    (mapcar (lambda (group)
              ;; use the last item of the group as the result item,
              ;; with additional property groupedItems
              (let (res)
                (setq res (cons `(groupedItems . ,group)
                                (car (last group))))
                res))
            (nreverse groups))))

(defun pr-review--insert-pr-body (pr diff)
  "Insert main body of PR with DIFF."
  (let ((top-comment-id-to-review-thread
         (pr-review--build-top-comment-id-to-review-thread-map pr))
        (timeline-items
         (pr-review--normalize-group-timeline-items (let-alist pr .timelineItems.nodes)))
        (status-check-rollup (let-alist
                                 (nth 0 (let-alist pr .latestCommits.nodes))
                               .commit.statusCheckRollup)))
    (let-alist pr
      (pr-review--insert-link .url .url)
      (insert "\n"
              (propertize .baseRefName 'face 'pr-review-branch-face)
              " <- "
              (propertize .headRefName 'face 'pr-review-branch-face))
      (pr-review--insert-labels-info pr)
      (insert "\n")
      (insert (pr-review--propertize-keyword .state)
              (if (equal .state "OPEN")
                  (concat " - " (pr-review--propertize-keyword .mergeable))
                "")
              " - "
              (propertize (concat "@" .author.login) 'face 'pr-review-author-face)
              " - "
              (pr-review--format-timestamp .createdAt)
              " - ")
      (pr-review--insert-subscription-info pr)
      (insert "\n\n")
      (pr-review--insert-reviewers-info pr)
      (pr-review--insert-assignees-info pr)
      (insert "\n")
      (magit-insert-section section (pr-review--description-section .id)
        (oset section body .body)
        (oset section updatable .viewerCanUpdate)
        (oset section reaction-groups .reactionGroups)
        (magit-insert-heading "Description")
        (pr-review--insert-html .bodyHTML)
        (pr-review--maybe-insert-reactions .reactionGroups))
      (insert "\n")
      (when (< .timelineItems.filteredCount .timelineItems.totalCount)
        (insert (propertize (format "Timeline items truncated. Displaying last %d of %d.\n"
                                    .timelineItems.filteredCount .timelineItems.totalCount)
                            'face 'pr-review-error-state-face))))
    (dolist (timeline-item timeline-items)
      (pcase (alist-get '__typename timeline-item)
        ("PullRequestReview"
         (pr-review--insert-review-section timeline-item top-comment-id-to-review-thread))
        ("IssueComment"
         (pr-review--insert-comment-section timeline-item))
        (_ (pr-review--insert-misc-event-section timeline-item))))
    (insert "\n")
    (let ((required-contexts (let-alist pr .baseRef.refUpdateRule.requiredStatusCheckContexts)))
      (when (or status-check-rollup required-contexts)
        (pr-review--insert-check-section status-check-rollup required-contexts)
        (insert "\n")))
    (let-alist pr
      (when .commits.nodes
        (pr-review--insert-commit-section .commits.nodes)
        (insert "\n")))
    (magit-insert-section (pr-review--diff-section)
      (magit-insert-heading
        (let-alist pr
          (concat (format "Files changed (%s files; %s additions, %s deleletions)"
                          (length .files.nodes)
                          (apply #'+ (mapcar (lambda (x) (alist-get 'additions x)) .files.nodes))
                          (apply #'+ (mapcar (lambda (x) (alist-get 'deletions x)) .files.nodes)))
                  (when pr-review--selected-commits
                    (format " - Only viewing selected %d commits" (length pr-review--selected-commits))))))
      (pr-review--insert-diff diff))
    (insert "\n")
    (pr-review--insert-review-action-buttons)
    (pr-review--insert-merge-close-reopen-action-buttons)
    (mapc 'pr-review--insert-in-diff-review-thread-link
          (let-alist pr .reviewThreads.nodes))
    (dolist (context (let-alist status-check-rollup .contexts.nodes))
      (when (equal (alist-get '__typename context) "CheckRun")
        (mapc 'pr-review--insert-in-diff-checkrun-annotation
              (let-alist context .annotations.nodes))))))

(defun pr-review--insert-pr (pr diff)
  "Insert pr buffer with PR and DIFF."
  (setq pr-review--char-pixel-width (shr-string-pixel-width "-"))
  (magit-insert-section section (pr-review--root-section)
    (let-alist pr
      (oset section title .title)
      (oset section updatable .viewerCanUpdate)
      (magit-insert-heading
        (propertize (alist-get 'title pr)'face 'pr-review-title-face)))
    (insert "\n")
    (pr-review--insert-pr-body pr diff))
  ;; need to call after this inserting all sections
  (pr-review--hide-generated-files))


(provide 'pr-review-render)
;;; pr-review-render.el ends here
