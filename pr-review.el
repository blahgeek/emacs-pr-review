;;; pr-review.el --- Review github PR    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yikai Zhao

;; Author: Yikai Zhao <yikai@z1k.dev>
;; Keywords: tools
;; Version: 0.1
;; URL: https://github.com/blahgeek/emacs-pr-review
;; Package-Requires: ((emacs "27.1") (magit-section "3.2") (magit "3.2") (markdown-mode "2.5") (ghub "3.5"))

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

;; Review github PR in EMACS.

;;; Code:

(require 'pr-review-common)
(require 'pr-review-api)
(require 'pr-review-input)
(require 'pr-review-render)
(require 'pr-review-action)
(require 'tabulated-list)

(defun pr-review--confirm-kill-buffer ()
  "Hook for `kill-buffer-query-functions', confirm if there's pending reviews."
  (or (null pr-review--pending-review-threads)
      (yes-or-no-p "Pending review threads exist in current buffer, really exit? ")))

(defvar pr-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "C-c C-r") #'pr-review-refresh)
    (define-key map (kbd "C-c C-c") #'pr-review-context-comment)
    (define-key map (kbd "C-c C-s") #'pr-review-context-action)
    (define-key map (kbd "C-c C-e") #'pr-review-context-edit)
    (define-key map (kbd "C-c C-v") #'pr-review-view-file)
    (define-key map (kbd "C-c C-f") #'pr-review-goto-file)
    (define-key map (kbd "C-c C-d") #'pr-review-ediff-file)
    (define-key map (kbd "C-c C-o") #'pr-review-open-in-default-browser)
    (define-key map (kbd "C-c C-q") #'pr-review-request-reviews)
    (define-key map (kbd "C-c C-l") #'pr-review-set-labels)
    map))

(defvar pr-review--mode-map-setup-for-evil-done nil)

(defun pr-review--mode-map-setup-for-evil ()
  "Setup map in `pr-review-mode-map' for evil mode (if loaded)."
  (when (and (fboundp 'evil-define-key*)
             (not pr-review--mode-map-setup-for-evil-done))
    (setq pr-review--mode-map-setup-for-evil-done t)
    (evil-define-key* '(normal motion) pr-review-mode-map
      (kbd "g r") #'pr-review-refresh
      (kbd "TAB") #'magit-section-toggle
      (kbd "z a") #'magit-section-toggle
      (kbd "z o") #'magit-section-show
      (kbd "z O") #'magit-section-show-children
      (kbd "z c") #'magit-section-hide
      (kbd "z C") #'magit-section-hide-children
      (kbd "z r") #'pr-review-increase-show-level
      (kbd "z R") #'pr-review-maximize-show-level
      (kbd "z m") #'pr-review-decrease-show-level
      (kbd "z M") #'pr-review-minimize-show-level
      (kbd "g h") #'magit-section-up
      (kbd "C-j") #'magit-section-forward
      (kbd "g j") #'magit-section-forward-sibling
      (kbd "C-k") #'magit-section-backward
      (kbd "g k") #'magit-section-backward-sibling
      (kbd "g f") #'pr-review-goto-file
      (kbd "g o") #'pr-review-open-in-default-browser
      [remap evil-previous-line] 'evil-previous-visual-line
      [remap evil-next-line] 'evil-next-visual-line
      (kbd "C-o") #'pop-to-mark-command
      (kbd "q") #'kill-current-buffer)))

(defvar-local pr-review--current-show-level 3)

(defun pr-review-increase-show-level ()
  "Increase the level of showing sections in current buffer.
Also see `magit-section-show-level'."
  (interactive)
  (when (< pr-review--current-show-level 4)
    (setq pr-review--current-show-level (1+ pr-review--current-show-level)))
  (magit-section-show-level (- pr-review--current-show-level)))

(defun pr-review-decrease-show-level ()
  "Decrease the level of showing sections in current buffer.
Also see `magit-section-show-level'."
  (interactive)
  (when (> pr-review--current-show-level 1)
    (setq pr-review--current-show-level (1- pr-review--current-show-level)))
  (magit-section-show-level (- pr-review--current-show-level)))

(defun pr-review-maximize-show-level ()
  "Set the level of showing sections to maximum in current buffer.
Which means that all sections are expanded."
  (interactive)
  (setq pr-review--current-show-level 4)
  (magit-section-show-level -4))

(defun pr-review-minimize-show-level ()
  "Set the level of showing sections to minimum in current buffer.
Which means that all sections are collapsed."
  (interactive)
  (setq pr-review--current-show-level 1)
  (magit-section-show-level -1))

(defun pr-review--eldoc-function (&rest _)
  "Hook for `eldoc-documentation-function', return content at current point."
  (get-text-property (point) 'pr-review-eldoc-content))

(define-derived-mode pr-review-mode magit-section-mode "PrReview"
  :interactive nil
  :group 'pr-review
  (pr-review--mode-map-setup-for-evil)
  (use-local-map pr-review-mode-map)
  (setq-local magit-hunk-section-map nil
              magit-file-section-map nil
              magit-diff-highlight-hunk-body nil)
  (add-to-list 'kill-buffer-query-functions 'pr-review--confirm-kill-buffer)
  (add-hook 'eldoc-documentation-functions #'pr-review--eldoc-function nil t)
  (eldoc-mode))

(defun pr-review--refresh-internal ()
  "Fetch and reload current PrReview buffer."
  (let* ((pr-info (pr-review--fetch-pr-info))
         (pr-diff (let-alist pr-info
                    (pr-review--fetch-compare-cached
                     .baseRefOid .headRefOid)))
         section-id)
    (setq-local pr-review--pr-info pr-info
                mark-ring nil)
    (when-let ((section (magit-current-section)))
      (setq section-id (oref section value)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pr-review--insert-pr pr-info pr-diff)
      (mapc (lambda (th) (pr-review--insert-in-diff-pending-review-thread
                          th 'allow-fallback))
            pr-review--pending-review-threads))
    (if section-id
        (pr-review--goto-section-with-value section-id)
      (goto-char (point-min)))
    (apply #'message "PR %s/%s/%s loaded" pr-review--pr-path)))

(defun pr-review-refresh (&optional clear-pending-reviews)
  "Fetch and reload current PrReview buffer.
If CLEAR-PENDING-REVIEWS is not nil, delete pending reviews if any,
otherwise, ask interactively."
  (interactive)
  (when (and pr-review--pending-review-threads
             (or clear-pending-reviews
                 (not (yes-or-no-p "Keep pending review threads (may not work if the changes are updated)? "))))
    (setq-local pr-review--pending-review-threads nil))
  (pr-review--refresh-internal))

;;;###autoload
(defun pr-review-url-parse (url)
  "Return pr path (repo-owner repo-name pr-id) for URL, or nil on error."
  (when-let* ((url-parsed (url-generic-parse-url url))
              (path (url-filename url-parsed)))
    (when (and (member (url-type url-parsed) '("http" "https"))
               (string-match (rx "/" (group (+ (any alphanumeric ?- ?_ ?.)))
                                 "/" (group (+ (any alphanumeric ?- ?_ ?.)))
                                 "/pull" (? "s") "/" (group (+ (any digit))))
                             (url-filename url-parsed)))
      (list (match-string 1 path)
            (match-string 2 path)
            (string-to-number (match-string 3 path))))))

(defun pr-review--url-parse-anchor (url)
  "Return anchor id for URL, or nil on error.
Example: given pr url https://github.com/.../pull/123#discussion_r12345,
return 12345 (as string).
This is used to jump to specific section after opening the buffer."
  (when-let ((fragment (cadr (split-string url "#"))))
    (when (string-match (rx (group (+ (any digit)))) fragment)
      (match-string 1 fragment))))

;;;###autoload
(defun pr-review-open (repo-owner repo-name pr-id &optional new-window anchor last-read-time)
  "Open review buffer for REPO-OWNER/REPO-NAME PR-ID (number).
Open in current window if NEW-WINDOW is nil, in other window otherwise.
ANCHOR is a database id that may be present in the url fragment
of a github pr notification, if it's not nil, try to jump to specific
location after open.
LAST-READ-TIME is the time when the PR is last read (in ISO string, mostly from notification buffer),
if it's not nil, newer comments will be highlighted, and it will jump to first unread comment
if ANCHOR is nil."
  (with-current-buffer (get-buffer-create (format "*pr-review %s/%s/%s*" repo-owner repo-name pr-id))
    (unless (eq major-mode 'pr-review-mode)
      (pr-review-mode))
    (setq-local pr-review--pr-path (list repo-owner repo-name pr-id))
    (let ((pr-review--last-read-time last-read-time))
      (pr-review-refresh))
    (unless (and anchor (pr-review-goto-database-id anchor))
      (when-let ((m (text-property-search-forward 'pr-review-unread t t)))
        (goto-char (prop-match-beginning m))))
    (funcall (if new-window
                 'switch-to-buffer-other-window
               'switch-to-buffer)
             (current-buffer))
    ;; for some known reason, recenter only works reliably after a redisplay
    (redisplay)
    (recenter)))

(defun pr-review--find-url-in-buffer ()
  "Return a possible pr url in current buffer.
It's used as the default value of `pr-review'."
  (or
   ;; url at point
   (when-let ((url (thing-at-point 'url t)))
     (when (pr-review-url-parse url)
       url))
   ;; find links in buffer. Useful in buffer with github notification emails
   (when-let ((prop (text-property-search-forward
                     'shr-url nil
                     (lambda (_ val) (and val (pr-review-url-parse val))))))
     (goto-char (prop-match-beginning prop))
     (prop-match-value prop))))

(defun pr-review--interactive-arg ()
  "Return args for interactive call for `pr-review'."
  (list
   ;; url
   (let* ((default-url (pr-review--find-url-in-buffer))
          (default-pr-path (and default-url (pr-review-url-parse default-url)))
          (input-url (read-string (concat "URL to review"
                                          (when default-pr-path
                                            (apply #'format " (default: %s/%s/%s)"
                                                   default-pr-path))
                                          ": "))))
     (if (string-empty-p input-url)
         (or default-url "")
       input-url))
   ;; new-window
   current-prefix-arg))

;;;###autoload
(defun pr-review (url &optional new-window)
  "Open Pr Review with URL (which is a link to github pr).
This is the main entrypoint of `pr-review'.
If NEW-WINDOW is not nil, open it in a new window.
When called interactively, user will be prompted to enter a PR url
and new window will be used when called with prefix."
  (interactive (pr-review--interactive-arg))
  (let ((res (pr-review-url-parse url))
        (anchor (pr-review--url-parse-anchor url)))
    (if (not res)
        (message "Cannot parse URL %s" url)
      (apply #'pr-review-open (append res (list new-window anchor))))))

;;;###autoload
(defun pr-review-open-url (url &optional new-window &rest _)
  "Open Pr Review with URL, in a new window if NEW-WINDOW is not nil.
This function is the same as `pr-review',
but it can be used in `browse-url-handlers' with `pr-review-url-parse'."
  (pr-review url new-window))


(provide 'pr-review)
;;; pr-review.el ends here
