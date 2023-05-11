;;; pr-review-listview.el --- Common list view mode for PRs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Yikai Zhao

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

(require 'tabulated-list)
(require 'pr-review-common)


(defface pr-review-listview-unread-face
  '((t :inherit bold))
  "Face used for unread notification rows."
  :group 'pr-review)

(defface pr-review-listview-read-face
  '((t :weight normal))
  "Face used for read notification&search rows."
  :group 'pr-review)

(defface pr-review-listview-unsubscribed-face
  '((t :inherit font-lock-comment-face))
  "Face used for unsubscribed notification&search rows."
  :group 'pr-review)

(defface pr-review-listview-status-face
  '((t :inherit font-lock-keyword-face))
  "Face used for PR status in notification&search list."
  :group 'pr-review)

(defface pr-review-listview-important-activity-face
  '((t :inherit font-lock-warning-face))
  "Face used for important activities in notification&search list."
  :group 'pr-review)

(defface pr-review-listview-unimportant-activity-face
  '((t :weight normal :slant italic))
  "Face used for unimportant activities in notification&search list."
  :group 'pr-review)


(defvar pr-review-listview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "C-c C-n") #'pr-review-listview-next-page)
    (define-key map (kbd "C-c C-p") #'pr-review-listview-prev-page)
    (define-key map (kbd "RET") #'pr-review-listview-open)
    map))

(defvar pr-review--listview-mode-map-setup-for-evil-done nil)

(defun pr-review--listview-mode-map-setup-for-evil ()
  "Setup map in `pr-review-listview-mode' for evil mode (if loaded)."
  (when (and (fboundp 'evil-define-key*)
             (not pr-review--listview-mode-map-setup-for-evil-done))
    (setq pr-review--listview-mode-map-setup-for-evil-done t)
    (evil-define-key* '(normal motion) pr-review-listview-mode-map
      (kbd "RET") #'pr-review-listview-open
      (kbd "gj") #'pr-review-listview-next-page
      (kbd "gk") #'pr-review-listview-prev-page
      (kbd "gn") #'pr-review-listview-goto-page
      (kbd "q") #'kill-current-buffer)))


(defvar-local pr-review--listview-page 1)
(defvar-local pr-review--listview-open-callback nil
  "Function to open an item in list view.  Accept one argument: the item.")

(define-derived-mode pr-review-listview-mode tabulated-list-mode
  "PrReviewListview"
  "Base mode for PR list view.
Derived modes must set the following variables:
- `tabulated-list-revert-hook'
- `pr-review--listview-open-callback'
And optional:
- `tabulated-list-printer'"
  :interactive nil
  :group 'pr-review
  (pr-review--listview-mode-map-setup-for-evil)
  (use-local-map pr-review-listview-mode-map))

(defun pr-review-listview-next-page ()
  "Go to next page of `pr-review-listview-mode'."
  (interactive)
  (unless (derived-mode-p 'pr-review-listview-mode)
    (error "Only available in pr-review-listview-mode"))
  (setq-local pr-review--listview-page (1+ pr-review--listview-page))
  (revert-buffer))

(defun pr-review-listview-prev-page ()
  "Go to previous page of `pr-review-listview-mode'."
  (interactive)
  (unless (derived-mode-p 'pr-review-listview-mode)
    (error "Only available in pr-review-listview-mode"))
  (when (> pr-review--listview-page 1)
    (setq-local pr-review--listview-page (1- pr-review--listview-page)))
  (revert-buffer))

(defun pr-review-listview-goto-page (page)
  "Go to page PAGE of `pr-review-listview-mode'."
  (interactive "nPage: ")
  (unless (derived-mode-p 'pr-review-listview-mode)
    (error "Only available in pr-review-listview-mode"))
  (setq-local pr-review--listview-page (max page 1))
  (revert-buffer))

(defun pr-review-listview-open ()
  "Open listview at current cursor."
  (interactive)
  (when-let ((entry (get-text-property (point) 'tabulated-list-id)))
    (when (functionp pr-review--listview-open-callback)
      (funcall pr-review--listview-open-callback entry))))


(defun pr-review--listview-format-time (time-str)
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


(provide 'pr-review-listview)
;;; pr-review-listview.el ends here
