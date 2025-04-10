;;; pr-review-notification.el --- Notification view for pr-review  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Yikai Zhao

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

(require 'pr-review-api)
(require 'pr-review-listview)
(require 'cl-seq)

(declare-function pr-review-open "pr-review")


(defcustom pr-review-notification-include-read t
  "Include read notifications."
  :type 'boolean
  :group 'pr-review)

(defcustom pr-review-notification-include-unsubscribed t
  "Include unsubscribed notifications."
  :type 'boolean
  :group 'pr-review)

(defvar pr-review-notification-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map pr-review-listview-mode-map)
    (define-key map (kbd "C-c C-t") #'pr-review-notification-toggle-filter)
    (define-key map (kbd "C-c C-u") #'pr-review-notification-remove-mark)
    (define-key map (kbd "C-c C-s") #'pr-review-notification-execute-mark)
    (define-key map (kbd "C-c C-r") #'pr-review-notification-mark-read)
    (define-key map (kbd "C-c C-d") #'pr-review-notification-mark-delete)
    (define-key map (kbd "C-c C-o") #'pr-review-notification-open-in-browser)
    map))

(defvar pr-review--notification-mode-map-setup-for-evil-done nil)

(defun pr-review--notification-mode-map-setup-for-evil ()
  "Setup map in `pr-review-notification-mode' for evil mode (if loaded)."
  (when (and (fboundp 'evil-define-key*)
             (not pr-review--notification-mode-map-setup-for-evil-done))
    (setq pr-review--notification-mode-map-setup-for-evil-done t)
    (evil-define-key* '(normal motion) pr-review-notification-mode-map
      (kbd "u") #'pr-review-notification-remove-mark
      (kbd "r") #'pr-review-notification-mark-read
      (kbd "d") #'pr-review-notification-mark-delete
      (kbd "x") #'pr-review-notification-execute-mark
      (kbd "o") #'pr-review-notification-open-in-browser)))

(define-derived-mode pr-review-notification-mode pr-review-listview-mode
  "PrReviewNotification"
  "Major mode for list of github notifications.

- Open item: `pr-review-listview-open'
  (While this buffer lists all types of notifications,
  only Pull Requests can be opened by this package).
- Page navigation: `pr-review-listview-next-page',
  `pr-review-listview-prev-page', `pr-review-listview-goto-page'
- Mark items as \"read\" or \"unsubscribed\" with
  `pr-review-notification-mark-read',`pr-review-notification-mark-delete',
  then use `pr-review-notification-execute-mark' to execute the marks.
  Remove existing mark with `pr-review-notification-remove-mark'.
- Toggle filter with `pr-review-notification-toggle-filter'.
- Refresh with `revert-buffer'

\\{pr-review-notification-mode-map}"
  :interactive nil
  :group 'pr-review
  (pr-review--notification-mode-map-setup-for-evil)
  (use-local-map pr-review-notification-mode-map)

  (add-hook 'tabulated-list-revert-hook #'pr-review--notification-refresh nil 'local)
  (add-to-list 'kill-buffer-query-functions 'pr-review--notification-confirm-kill-buffer)

  (setq-local pr-review--listview-open-callback #'pr-review--notification-open
              tabulated-list-printer #'pr-review--notification-print-entry
              tabulated-list-use-header-line nil
              tabulated-list-padding 2))

(defun pr-review--notification-entry-sort-updated-at (a b)
  "Sort tabulated list entries by timestamp for A and B."
  (string< (alist-get 'updated_at (car a)) (alist-get 'updated_at (car b))))

;; list of (id type last_updated)
;; type is one of: 'read 'delete
;; last_updated is used to filter outdated marks
(defvar-local pr-review--notification-marks nil)

(defun pr-review--notification-mark (entry)
  "Return mark for ENTRY.
Return one of \='read, \='delete, nil."
  (let ((id (alist-get 'id entry)))
    (nth 1 (seq-find (lambda (item) (equal (nth 0 item) id)) pr-review--notification-marks))))

(defun pr-review--notification-confirm-kill-buffer ()
  "Hook for `kill-buffer-query-functions'.
Confirm if there's mark entries."
  (or (null pr-review--notification-marks)
      (yes-or-no-p (substitute-command-keys
                    "Marked entries exist in current buffer (use `\\[pr-review-notification-execute-mark]' to execute), really exit? "))))

(defun pr-review--notification-print-entry (entry cols)
  "Print ENTRY with COLS for tabulated-list, with custom properties."
  (let ((beg (point)))
    (tabulated-list-print-entry entry cols)
    (save-excursion
      (goto-char beg)  ;; we are already in the next line
      (tabulated-list-put-tag
       (pcase (pr-review--notification-mark entry)
         ('read "-")
         ('delete "D")
         (_ ""))))
    (if (alist-get 'unread entry)
        (add-face-text-property beg (point) 'pr-review-listview-unread-face 'append)
      (add-face-text-property beg (point) 'pr-review-listview-read-face))  ;; for read-face, its priority is higher. do not append
    (when (pr-review--notification-unsubscribed entry)
      (add-face-text-property beg (point) 'pr-review-listview-unsubscribed-face))
    (pulse-momentary-highlight-region 0 (point))))

(defun pr-review--notification-format-type (entry)
  "Format type column of notification ENTRY."
  (let-alist entry
    (pcase .subject.type
      ("PullRequest" "PR")
      ("Issue" "ISS")
      (_ .subject.type))))

(defun pr-review--notification-unsubscribed (entry)
  "Return the subscription state if ENTRY is unsubscribed, nil if subscribed."
  (let-alist entry
    (when (and .pr-info.viewerSubscription
               (not (equal .pr-info.viewerSubscription "SUBSCRIBED")))
      .pr-info.viewerSubscription)))

(defun pr-review--notification-format-activities (entry)
  "Format activities for notification ENTRY."
  (let ((my-login (let-alist (pr-review--whoami-cached) .viewer.login))
        (op (let-alist entry .pr-info.author.login))
        ;; for the following me-* status: t means yes, 'new means yes+new
        me-mentioned me-assigned me-review-requested me-approved
        new-participants all-participants
        all-reviewers approved-reviewers rejected-reviewers)
    (let-alist entry
      (when op
        (push op all-participants)
        (unless .last_read_at
          ;; add author to commenters if no last read
          (push op new-participants)))
      (dolist (opinionated-review .pr-info.latestOpinionatedReviews.nodes)
        (let-alist opinionated-review
          (pcase .state
            ("APPROVED" (push .author.login approved-reviewers))
            ("CHANGES_REQUESTED" (push .author.login rejected-reviewers)))))
      (setq all-reviewers (mapcar (lambda (n) (let-alist n .requestedReviewer.login)) .pr-info.reviewRequests.nodes)
            me-assigned (cl-find-if (lambda (node) (equal my-login (let-alist node .login)))
                                    .pr-info.assignees.nodes)
            me-review-requested (member my-login all-reviewers)
            me-approved (member my-login approved-reviewers)))
    (dolist (timeline-item (let-alist entry .pr-info.timelineItemsSince.nodes))
      (let-alist timeline-item
        (pcase .__typename
          ("AssignedEvent" (when (equal my-login .assignee.login)
                             (setq me-assigned 'new)))
          ("ReviewRequestedEvent" (when (and (equal my-login .requestedReviewer.login) (not me-approved))
                                    (setq me-review-requested 'new)))
          ("MentionedEvent" (when (equal my-login .actor.login)
                              (setq me-mentioned t)))
          ((or "IssueComment" "PullRequestReview")
           (unless (equal my-login .author.login)
             (push .author.login new-participants)))
          )))
    (dolist (participant-item (let-alist entry .pr-info.participants.nodes))
      (let ((login (let-alist participant-item .login)))
        (unless (or (equal login my-login) (member login new-participants))
          (push login all-participants))))
    (setq all-participants (delete-dups (append (reverse new-participants)
                                                (reverse all-participants))))
    (concat (let-alist entry
              (when (and .pr-info.state (not (equal .pr-info.state "OPEN")))
                (concat (propertize (downcase .pr-info.state) 'face 'pr-review-listview-status-face) " ")))
            (when me-mentioned (propertize "+mentioned " 'face 'pr-review-listview-important-activity-face))
            (pcase me-assigned
              ('new (propertize "+assigned " 'face 'pr-review-listview-important-activity-face))
              ('t (propertize "assigned " 'face 'pr-review-listview-status-face)))
            (pcase me-review-requested
             ('new (propertize "+review_requested " 'face 'pr-review-listview-important-activity-face))
             ('t (propertize "review_requested " 'face 'pr-review-listview-status-face)))
            (when me-approved
              (propertize "approved " 'face 'pr-review-listview-status-face))
            (when all-participants
              (mapconcat
               (lambda (x)
                 (let ((is-new (member x new-participants)))
                   (propertize
                    (concat
                     (when is-new "+")
                     x
                     (cond
                      ((equal x op) "@")
                      ((member x approved-reviewers) "#")
                      ((member x rejected-reviewers) "!")
                      ((member x all-reviewers) "?")))
                    'face
                    (if is-new nil 'pr-review-listview-unimportant-activity-face))))
               all-participants " ")))))

(defun pr-review--notification-refresh ()
  "Refresh notification buffer."
  (unless (eq major-mode 'pr-review-notification-mode)
    (error "Only available in pr-review-notification-mode"))

  (setq-local tabulated-list-format
              [("Updated at" 12 pr-review--notification-entry-sort-updated-at)
               ("Type" 4 t)
               ("Title" 85 nil)
               ("Activities" 25 nil)])
  (let* ((resp-orig (pr-review--get-notifications-with-extra-pr-info
                     pr-review-notification-include-read
                     pr-review--listview-page))
         (resp resp-orig))
    (unless pr-review-notification-include-unsubscribed
      ;; TODO: handle Issue
      (setq resp (seq-filter (lambda (item) (not (pr-review--notification-unsubscribed item)))
                             resp)))
    (setq-local header-line-format
                (substitute-command-keys
                 (format "Page %d, %d items. Filter: %s %s"
                         pr-review--listview-page
                         (length resp)
                         (if pr-review-notification-include-read "+read" "-read")
                         (if pr-review-notification-include-unsubscribed "+unsubscribed"
                           (format "-unsubscribed (%d filtered)" (- (length resp-orig) (length resp)))))))
    ;; refresh marks, remove those with outdated last_updated
    (let ((current-last-updated (make-hash-table :test 'equal)))
      (dolist (entry resp)
        (let-alist entry
          (puthash .id .updated_at current-last-updated)))
      (setq-local pr-review--notification-marks
                  (seq-filter (lambda (item) (equal (nth 2 item)
                                                    (gethash (nth 0 item) current-last-updated)))
                              pr-review--notification-marks)))
    (setq-local
     tabulated-list-entries
     (mapcar (lambda (entry)
               (let-alist entry
                 (list entry
                       (vector
                        (pr-review--listview-format-time .updated_at)
                        (pr-review--notification-format-type entry)
                        (format "[%s] %s" .repository.full_name (string-trim-right .subject.title))
                        (pr-review--notification-format-activities entry)
                        ;; .reason
                        ))))
             resp))
    (tabulated-list-init-header)
    (message (concat (format "Notifications refreshed, %d items." (length resp))
                     (when (> (length resp-orig) (length resp))
                       (format " (filtered %d unsubscribed items)" (- (length resp-orig) (length resp))))))))

(defun pr-review-notification-toggle-filter ()
  "Toggle filter of `pr-review-notification-mode'."
  (interactive)
  (unless (eq major-mode 'pr-review-notification-mode)
    (error "Only available in pr-review-notification-mode"))
  (let ((ans (completing-read "Filter: " '("+read +unsubscribed"
                                           "+read -unsubscribed"
                                           "-read -unsubscribed"
                                           "-read +unsubscribed")
                              nil 'require-match)))
    (setq-local pr-review-notification-include-read (string-match-p (rx "+read") ans)
                pr-review-notification-include-unsubscribed (string-match-p (rx "+unsubscribed") ans)))
  (revert-buffer))

(defun pr-review-notification-remove-mark ()
  "Remove any mark of the entry in current line."
  (interactive)
  (when-let ((entry (get-text-property (point) 'tabulated-list-id)))
    (when (pr-review--notification-mark entry)
      (setq-local pr-review--notification-marks
                  (cl-remove-if (lambda (elem) (equal (car elem) (alist-get 'id entry)))
                                pr-review--notification-marks))
      (tabulated-list-put-tag ""))
    entry))

(defun pr-review-notification-mark-read ()
  "Mark the entry in current line as read."
  (interactive)
  (when-let ((entry (pr-review-notification-remove-mark)))
    (let-alist entry
      (push (list .id 'read .updated_at) pr-review--notification-marks)
      (tabulated-list-put-tag "-"))
    (forward-line)))

(defun pr-review-notification-mark-delete ()
  "Mark the entry in current line as delete."
  (interactive)
  (when-let ((entry (pr-review-notification-remove-mark)))
    (let-alist entry
      (push (list .id 'delete .updated_at) pr-review--notification-marks)
      (tabulated-list-put-tag "D"))
    (forward-line)))

(defun pr-review-notification-execute-mark ()
  "Really execute all mark."
  (interactive)
  (dolist (mark pr-review--notification-marks)
    (pcase (nth 1 mark)
      ('read (pr-review--mark-notification-read (car mark)))
      ;; NOTE: github does not really allow to mark the notification as done/deleted, like in the web interface
      ;; what this API actually does is to mark the notification as unsubscribed.
      ;; in order to make this work, we would not display unsubscribed threads by default. See "filter" above
      ('delete (pr-review--delete-notification (car mark)))))
  (setq-local pr-review--notification-marks nil)
  (revert-buffer))

(defun pr-review--notification-open (entry)
  "Open notification ENTRY."
  (let-alist entry
    (when (and .unread
               (not (pr-review--notification-mark entry)))  ;; do not alter mark
      (push (list .id 'read .updated_at) pr-review--notification-marks)
      (tabulated-list-put-tag "-"))
    (if (equal .subject.type "PullRequest")
        (let ((pr-id (when (string-match (rx (group (+ (any digit))) eos) .subject.url)
                       (match-string 1 .subject.url))))
          (pr-review-open .repository.owner.login .repository.name
                          (string-to-number pr-id)
                          nil  ;; new window
                          nil  ;; anchor nil; do not go to latest comment, use last_read_at
                          .last_read_at))
      (browse-url .subject.url))))

(defun pr-review-notification-open-in-browser ()
  "Open current notification entry in browser."
  (interactive)
  (when-let ((entry (get-text-property (point) 'tabulated-list-id)))
    (let-alist entry
      (browse-url-with-browser-kind 'external .subject.url))))

;;;###autoload
(defun pr-review-notification ()
  "Show github notifications in a new buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*pr-review notifications*")
    (pr-review-notification-mode)
    (pr-review--notification-refresh)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(provide 'pr-review-notification)
;;; pr-review-notification.el ends here
