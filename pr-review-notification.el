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
(require 'tabulated-list)
(require 'cl-seq)

(declare-function pr-review-open "pr-review")

(defface pr-review-notification-unread-face
  '((t :inherit bold))
  "Face used for unread notification rows."
  :group 'pr-review)

(defvar-local pr-review--notification-page 1)
(defvar-local pr-review--notification-include-read t)

(defvar pr-review-notification-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "C-c C-n") #'pr-review-notification-next-page)
    (define-key map (kbd "C-c C-p") #'pr-review-notification-prev-page)
    (define-key map (kbd "C-c C-t") #'pr-review-notification-toggle-unread-filter)
    (define-key map (kbd "C-c C-u") #'pr-review-notification-remove-pending-mark-read)
    (define-key map (kbd "C-c C-s") #'pr-review-notification-execute-mark-read)
    (define-key map (kbd "RET") #'pr-review-notification-open)
    map))

(defvar pr-review--notification-mode-map-setup-for-evil-done nil)

(defun pr-review--notification-mode-map-setup-for-evil ()
  "Setup map in `pr-review-notification-list-mode' for evil mode (if loaded)."
  (when (and (fboundp 'evil-define-key*)
             (not pr-review--notification-mode-map-setup-for-evil-done))
    (setq pr-review--notification-mode-map-setup-for-evil-done t)
    (evil-define-key* '(normal motion) pr-review-notification-list-mode-map
      (kbd "RET") #'pr-review-notification-open
      (kbd "C-j") #'pr-review-notification-next-page
      (kbd "C-k") #'pr-review-notification-prev-page
      (kbd "u") #'pr-review-notification-remove-pending-mark-read
      (kbd "x") #'pr-review-notification-execute-mark-read
      (kbd "q") #'kill-current-buffer)))

(define-derived-mode pr-review-notification-list-mode tabulated-list-mode
  "GithubNotifications"
  :interactive nil
  :group 'pr-review
  (pr-review--notification-mode-map-setup-for-evil)
  (use-local-map pr-review-notification-list-mode-map)
  (setq-local pr-review--notification-page 1
              pr-review--notification-include-read t)
  (setq-local tabulated-list-format
              [("Repo" 25 t)
               ("Type" 12 t)
               ("Title" 65 nil)
               ("Updated at" 22 pr-review--notification-entry-sort-updated-at)
               ("Reason" 15 t)])

  (add-hook 'tabulated-list-revert-hook #'pr-review--notification-refresh)
  (add-to-list 'kill-buffer-query-functions 'pr-review--notification-confirm-kill-buffer)

  (setq-local tabulated-list-printer #'pr-review--notification-print-entry
              tabulated-list-use-header-line nil
              tabulated-list-padding 2))

(defun pr-review--notification-entry-sort-updated-at (a b)
  "Sort tabulated list entries by timestamp for A and B."
  (string< (alist-get 'updated_at (car a)) (alist-get 'updated_at (car b))))

;; list of (id, last_updated)
(defvar-local pr-review--notification-pending-mark-read nil)

(defun pr-review--notification-is-pending-mark-read (entry)
  "Check if ENTRY is pending mark as read."
  (and (alist-get 'unread entry)
       (member (let-alist entry (list .id .updated_at)) pr-review--notification-pending-mark-read)))

(defun pr-review--notification-get-pending-mark-read-ids ()
  "Get the list of ids that is pending mark as read."
  (let ((result))
    (mapc (lambda (item)
            (when (pr-review--notification-is-pending-mark-read (car item))
              (push (alist-get 'id (car item)) result)))
          tabulated-list-entries)
    result))

(defun pr-review--notification-confirm-kill-buffer ()
  "Hook for `kill-buffer-query-functions'.
Confirm if there's pending mark read entries."
  (or (null pr-review--notification-pending-mark-read)
      (null (pr-review--notification-get-pending-mark-read-ids))
      (yes-or-no-p (substitute-command-keys
                    "Pending to-mark-as-read entries exist in current buffer (use `\\[pr-review-notification-execute-mark-read]' to execute), really exit? "))))

(defun pr-review--notification-print-entry (entry cols)
  "Print ENTRY with COLS for tabulated-list, with custom properties."
  (let ((beg (point)))
    (tabulated-list-print-entry entry cols)
    (when (alist-get 'unread entry)
      (save-excursion
        (goto-char beg)  ;; we are already in the next line
        (if (pr-review--notification-is-pending-mark-read entry)
            (tabulated-list-put-tag "-")
          (tabulated-list-put-tag "*")))
      (add-face-text-property beg (point) 'pr-review-notification-unread-face))))


(defun pr-review--notification-format-type (entry)
  "Format type column of notification ENTRY."
  (let-alist entry
    (if (not (equal .subject.type "PullRequest"))
        .subject.type
      (format "PR %s" .pr-info.state))))

(defun pr-review--notification-format-notable-activities (entry)
  "Format 'notable activities since last read' column for notification ENTRY."
  (let ((my-login (let-alist (pr-review--whoami-cached) .viewer.login))
        mentioned assigned review-requested commenters)
    (dolist (timeline-item (let-alist entry .pr-info.timelineItemsSince.nodes))
      (let-alist timeline-item
        (pcase .__typename
          ("AssignedEvent" (when (equal my-login .assignee.login)
                             (setq assigned t)))
          ("ReviewRequestedEvent" (when (equal my-login .requestedReviewer.login)
                                    (setq review-requested t)))
          ("MentionedEvent" (when (equal my-login .actor.login)
                              (setq mentioned t)))
          ((or "IssueComment" "PullRequestReview")
           (unless (equal my-login .author.login)
             (push .author.login commenters)))
          )))
    (concat (when assigned "ASSIGNED ")
            (when mentioned "MENTIONED ")
            (when review-requested "REVIEW-REQUESTED ")
            (when commenters
              (format "COMMENTED (%s)" (string-join commenters ", "))))))

(defun pr-review--notification-refresh ()
  "Refresh notification buffer."
  (unless (eq major-mode 'pr-review-notification-list-mode)
    (error "Only available in pr-review-notification-list-mode"))
  (let ((resp (pr-review--get-notifications-with-extra-pr-info
               pr-review--notification-include-read
               pr-review--notification-page)))
    (setq-local header-line-format
                (substitute-command-keys
                 (format "Page %d, %d items, %s. Go next/prev page with `\\[pr-review-notification-next-page]'/`\\[pr-review-notification-prev-page]'. Toggle unread filter with `\\[pr-review-notification-toggle-unread-filter]'"
                         pr-review--notification-page
                         (length resp)
                         (if pr-review--notification-include-read "no filter" "unread only"))))
    (setq-local
     tabulated-list-entries
     (mapcar (lambda (entry)
               (let-alist entry
                 (list entry
                       (vector
                        .repository.full_name
                        (pr-review--notification-format-type entry)
                        .subject.title
                        (format-time-string "%b %d, %Y, %H:%M" (date-to-time .updated_at))
                        (pr-review--notification-format-notable-activities entry)
                        ;; .reason
                        ))))
             resp))
    (tabulated-list-init-header)
    (message "Notifications refreshed, %d items." (length resp))))

(defun pr-review-notification-next-page ()
  "Go to next page of `pr-review-notification-list-mode'."
  (interactive)
  (unless (eq major-mode 'pr-review-notification-list-mode)
    (error "Only available in pr-review-notification-list-mode"))
  (setq-local pr-review--notification-page (1+ pr-review--notification-page))
  (revert-buffer))

(defun pr-review-notification-prev-page ()
  "Go to previous page of `pr-review-notification-list-mode'."
  (interactive)
  (unless (eq major-mode 'pr-review-notification-list-mode)
    (error "Only available in pr-review-notification-list-mode"))
  (when (> pr-review--notification-page 1)
    (setq-local pr-review--notification-page (1- pr-review--notification-page)))
  (revert-buffer))

(defun pr-review-notification-toggle-unread-filter ()
  "Toggle unread filter of `pr-review-notification-list-mode'."
  (interactive)
  (unless (eq major-mode 'pr-review-notification-list-mode)
    (error "Only available in pr-review-notification-list-mode"))
  (setq-local pr-review--notification-include-read (not pr-review--notification-include-read))
  (revert-buffer))

(defun pr-review-notification-remove-pending-mark-read ()
  "Remove pending mark as read of the entry in current line."
  (interactive)
  (when-let ((entry (get-text-property (point) 'tabulated-list-id)))
    (unless (pr-review--notification-is-pending-mark-read entry)
      (error "You can only remove pending read entries' marks, you cannot mark read notifications as unread"))
    (setq-local pr-review--notification-pending-mark-read
                (cl-remove-if (lambda (elem) (equal (car elem) (alist-get 'id entry)))
                              pr-review--notification-pending-mark-read))
    (tabulated-list-put-tag "*")))

(defun pr-review-notification-execute-mark-read ()
  "Really mark pending entries as read."
  (interactive)
  (mapc #'pr-review--mark-notification-read
        (pr-review--notification-get-pending-mark-read-ids))
  (setq-local pr-review--notification-pending-mark-read nil)
  (revert-buffer))

(defun pr-review-notification-open ()
  "Open notification at current cursor."
  (interactive)
  (when-let ((entry (get-text-property (point) 'tabulated-list-id)))
    (let-alist entry
      (when (and .unread
                 (not (pr-review--notification-is-pending-mark-read entry)))
        (push (list .id .updated_at) pr-review--notification-pending-mark-read)
        (tabulated-list-put-tag "-"))
      (if (equal .subject.type "PullRequest")
          (let ((pr-id (when (string-match (rx (group (+ (any digit))) eos) .subject.url)
                         (match-string 1 .subject.url)))
                (anchor (when (string-match (rx (group (+ (any digit))) eos) (or .subject.latest_comment_url ""))
                          (match-string 1 .subject.latest_comment_url))))
            (pr-review-open .repository.owner.login .repository.name
                            (string-to-number pr-id)
                            nil  ;; new window
                            anchor))
        (browse-url .subject.url)))))

(provide 'pr-review-notification)
;;; pr-review-notification.el ends here
