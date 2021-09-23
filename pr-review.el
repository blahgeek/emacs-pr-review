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


(defvar pr-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "C-c C-r") #'pr-review-refresh)
    map))

(define-derived-mode pr-review-mode magit-section-mode "PrReview"
  :interactive nil
  :group 'pr-review
  (use-local-map pr-review-mode-map)
  (when (fboundp 'evil-define-key)
    (evil-define-key '(normal motion) 'local
      (kbd "TAB") 'magit-section-toggle
      [remap evil-previous-line] 'evil-previous-visual-line
      [remap evil-next-line] 'evil-next-visual-line)))

(defun pr-review-refresh ()
  (interactive)
  (let* ((pr-info (apply 'pr-review--fetch-pr-info pr-review--pr-path))
         (repo-owner (car pr-review--pr-path))
         (repo-name (cadr pr-review--pr-path))
         (pr-diff (let-alist pr-info
                    (pr-review--fetch-compare-cached
                     repo-owner repo-name .baseRefOid .headRefOid)))
         section-id)
    (setq-local pr-review--pr-node-id (alist-get 'id pr-info))
    (when-let ((section (magit-current-section)))
      (setq section-id (oref section value)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pr-review--insert-pr pr-info pr-diff))
    (if section-id
        (pr-review--goto-section-with-value section-id)
      (beginning-of-buffer))
    (apply 'message "PR %s/%s/%s ready for review" pr-review--pr-path)))

(defun pr-review-open-parsed (repo-owner repo-name pr-id)
  (with-current-buffer (get-buffer-create (format "*pr-review %s/%s/%s*" repo-owner repo-name pr-id))
    (unless (eq major-mode 'pr-review-mode)
      (pr-review-mode))
    (setq-local pr-review--pr-path (list repo-owner repo-name pr-id))
    (pr-review-refresh)
    (switch-to-buffer-other-window (current-buffer))))



(provide 'pr-review)
;;; pr-review.el ends here
