;;; pr-review.el --- Review github PR in emacs.    -*- lexical-binding: t; -*-

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

;;; Code:

(require 'pr-review-common)
(require 'pr-review-api)
(require 'pr-review-input)
(require 'pr-review-render)
(require 'pr-review-action)


(defun pr-review--confirm-kill-buffer ()
  (or (null pr-review--pending-review-threads)
      (yes-or-no-p "Pending review threads exist in current buffer, really exit? ")))

(defvar pr-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "C-c C-r") #'pr-review-refresh)
    (define-key map (kbd "C-c C-o") #'markdown-follow-link-at-point)
    map))

(define-derived-mode pr-review-mode magit-section-mode "PrReview"
  :interactive nil
  :group 'pr-review
  (use-local-map pr-review-mode-map)
  (setq-local magit-hunk-section-map pr-review--diff-section-map
              magit-file-section-map pr-review--diff-section-map
              magit-diff-highlight-hunk-body nil)
  (add-to-list 'kill-buffer-query-functions 'pr-review--confirm-kill-buffer)
  (when (fboundp 'evil-define-key)
    (evil-define-key '(normal motion) 'local
      (kbd "TAB") 'magit-section-toggle
      [remap evil-previous-line] 'evil-previous-visual-line
      [remap evil-next-line] 'evil-next-visual-line)))

(defun pr-review--refresh-internal ()
  "Fetch and reload current PrReview buffer."
  (let* ((pr-info (apply 'pr-review--fetch-pr-info pr-review--pr-path))
         (repo-owner (car pr-review--pr-path))
         (repo-name (cadr pr-review--pr-path))
         (pr-diff (let-alist pr-info
                    (pr-review--fetch-compare-cached
                     repo-owner repo-name .baseRefOid .headRefOid)))
         section-id)
    (let-alist pr-info
      (setq-local pr-review--pr-node-id .id
                  pr-review--head-commit-id .headRefOid
                  pr-review--pending-review-threads nil))
    (when-let ((section (magit-current-section)))
      (setq section-id (oref section value)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pr-review--insert-pr pr-info pr-diff))
    (if section-id
        (pr-review--goto-section-with-value section-id)
      (beginning-of-buffer))
    (apply 'message "PR %s/%s/%s loaded" pr-review--pr-path)))

(defun pr-review-refresh (&optional force)
  "Fetch and reload current PrReview buffer, if FORCE is nil, ask confirmation when there's pending reviews."
  (interactive)
  (when (or (null pr-review--pending-review-threads)
            force
            (yes-or-no-p "Pending review threads exist in current buffer, really refresh? "))
    (pr-review--refresh-internal)))

(defun pr-review-open-parsed (repo-owner repo-name pr-id)
  (with-current-buffer (get-buffer-create (format "*pr-review %s/%s/%s*" repo-owner repo-name pr-id))
    (unless (eq major-mode 'pr-review-mode)
      (pr-review-mode))
    (setq-local pr-review--pr-path (list repo-owner repo-name pr-id))
    (pr-review-refresh)
    (switch-to-buffer-other-window (current-buffer))))


(defun pr-review-open (url)
  (interactive "s")
  (let ((match (string-match (rx "http" (? "s") "://github.com/"
                                 (group (+ (not ?/))) "/"
                                 (group (+ (not ?/))) "/pull/"
                                 (group (+ (any digit))))
                             url)))
    (if (not match)
        (message "Cannot parse URL %s" url)
      (pr-review-open-parsed (match-string 1 url)
                             (match-string 2 url)
                             (string-to-number (match-string 3 url))))))



(provide 'pr-review)
;;; pr-review.el ends here
