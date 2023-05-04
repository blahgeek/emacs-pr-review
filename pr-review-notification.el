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

(defface pr-review-notification-read-face
  '((t :weight normal))
  "Face used for read notification rows."
  :group 'pr-review)

(defface pr-review-notification-unsubscribed-face
  '((t :inherit font-lock-comment-face))
  "Face used for unsubscribed notification rows."
  :group 'pr-review)

(defface pr-review-notification-tag-face
  '((t :inherit font-lock-warning-face))
  "Face used for tags in notification list."
  :group 'pr-review)


(defvar-local pr-review--notification-page 1)
(defvar-local pr-review-notification-include-read t)
(defvar-local pr-review-notification-include-unsubscribed t)

(defvar pr-review-notification-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "C-c C-n") #'pr-review-notification-next-page)
    (define-key map (kbd "C-c C-p") #'pr-review-notification-prev-page)
    (define-key map (kbd "C-c C-t") #'pr-review-notification-toggle-filter)
    (define-key map (kbd "C-c C-u") #'pr-review-notification-remove-mark)
    (define-key map (kbd "C-c C-s") #'pr-review-notification-execute-mark)
    (define-key map (kbd "C-c C-r") #'pr-review-notification-mark-read)
    (define-key map (kbd "C-c C-d") #'pr-review-notification-mark-delete)
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
      (kbd "gj") #'pr-review-notification-next-page
      (kbd "gk") #'pr-review-notification-prev-page
      (kbd "gn") #'pr-review-notification-goto-page
      (kbd "u") #'pr-review-notification-remove-mark
      (kbd "r") #'pr-review-notification-mark-read
      (kbd "d") #'pr-review-notification-mark-delete
      (kbd "x") #'pr-review-notification-execute-mark
      (kbd "q") #'kill-current-buffer)))

(define-derived-mode pr-review-notification-list-mode tabulated-list-mode
  "GithubNotifications"
  :interactive nil
  :group 'pr-review
  (pr-review--notification-mode-map-setup-for-evil)
  (use-local-map pr-review-notification-list-mode-map)

  (add-hook 'tabulated-list-revert-hook #'pr-review--notification-refresh)
  (add-to-list 'kill-buffer-query-functions 'pr-review--notification-confirm-kill-buffer)

  (setq-local tabulated-list-printer #'pr-review--notification-print-entry
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
Return one of 'read, 'delete, nil."
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
    (add-face-text-property
     beg (point)
     (if (alist-get 'unread entry)
         'pr-review-notification-unread-face
       'pr-review-notification-read-face))
    (when (pr-review--notification-unsubscribed entry)
      (add-face-text-property beg (point) 'pr-review-notification-unsubscribed-face))
    (pulse-momentary-highlight-region 0 (point))))

(defun pr-review--notification-format-type (entry)
  "Format type column of notification ENTRY."
  (let-alist entry
    (if (not (equal .subject.type "PullRequest"))
        .subject.type
      "PullReq")))

(defun pr-review--notification-unsubscribed (entry)
  "Return the subscription state if ENTRY is unsubscribed, nil if subscribed."
  (let-alist entry
    (when (and .pr-info.viewerSubscription
               (not (equal .pr-info.viewerSubscription "SUBSCRIBED")))
      .pr-info.viewerSubscription)))

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
    (concat (let-alist entry
              (when (not (equal .pr-info.state "OPEN"))
                (concat (propertize (downcase .pr-info.state) 'face 'pr-review-notification-tag-face) " ")))
            (when assigned (propertize "+assisnged " 'face 'pr-review-notification-tag-face))
            (when mentioned (propertize "+mentioned " 'face 'pr-review-notification-tag-face))
            (when review-requested (propertize "+review_requested " 'face 'pr-review-notification-tag-face))
            (when commenters
              (format "+{%s}" (string-join (delete-dups commenters) ", "))))))

(defun pr-review--notification-format-time (time-str)
  "Format TIME-STR as human readable relative string."
  (let* ((time (date-to-time time-str))
         (delta (float-time (time-subtract (current-time) time))))
    (cond
     ((< delta 3600)
      (format "%.0f min. ago" (/ delta 60)))
     ((equal (time-to-days time) (time-to-days (current-time)))
      (format-time-string "Today %H:%M" time))
     ((< delta (* 5 24 3600))
      (format-time-string "%a. %H:%M" time))
     ((< delta (* 365 24 3600))
      (format-time-string "%b %d" time))
     (t
      (format-time-string "%b %d, %Y" time)))))

(defun pr-review--notification-refresh ()
  "Refresh notification buffer."
  (unless (eq major-mode 'pr-review-notification-list-mode)
    (error "Only available in pr-review-notification-list-mode"))

  (setq-local tabulated-list-format
              [("Updated at" 12 pr-review--notification-entry-sort-updated-at)
               ("Type" 8 t)
               ("Title" 85 nil)
               ("Activities" 25 nil)])
  (let* ((resp-orig (pr-review--get-notifications-with-extra-pr-info
                     pr-review-notification-include-read
                     pr-review--notification-page))
         (resp resp-orig))
    (unless pr-review-notification-include-unsubscribed
      ;; TODO: handle Issue
      (setq resp (seq-filter (lambda (item) (not (pr-review--notification-unsubscribed item)))
                             resp)))
    (setq-local header-line-format
                (substitute-command-keys
                 (format "Page %d, %d items. Filter: %s %s. Go next/prev page with `\\[pr-review-notification-next-page]'/`\\[pr-review-notification-prev-page]'. Toggle filter with `\\[pr-review-notification-toggle-filter]'"
                         pr-review--notification-page
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
                        (pr-review--notification-format-time .updated_at)
                        (pr-review--notification-format-type entry)
                        (format "[%s] %s" .repository.full_name .subject.title)
                        (pr-review--notification-format-notable-activities entry)
                        ;; .reason
                        ))))
             resp))
    (tabulated-list-init-header)
    (message (concat (format "Notifications refreshed, %d items." (length resp))
                     (when (> (length resp-orig) (length resp))
                       (format " (filtered %d unsubscribed items)" (- (length resp-orig) (length resp))))))))

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

(defun pr-review-notification-goto-page (page)
  "Go to page PAGE of `pr-review-notification-list-mode'."
  (interactive "nPage: ")
  (unless (eq major-mode 'pr-review-notification-list-mode)
    (error "Only available in pr-review-notification-list-mode"))
  (setq-local pr-review--notification-page (max page 1))
  (revert-buffer))

(defun pr-review-notification-toggle-filter ()
  "Toggle filter of `pr-review-notification-list-mode'."
  (interactive)
  (unless (eq major-mode 'pr-review-notification-list-mode)
    (error "Only available in pr-review-notification-list-mode"))
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

(defun pr-review-notification-open ()
  "Open notification at current cursor."
  (interactive)
  (when-let ((entry (get-text-property (point) 'tabulated-list-id)))
    (let-alist entry
      (when (and .unread
                 (not (pr-review--notification-mark entry)))  ;; do not alter mark
        (push (list .id 'read .updated_at) pr-review--notification-marks)
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
