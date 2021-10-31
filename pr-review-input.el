;;; pr-review-input.el --- Input functions for pr-review  -*- lexical-binding: t; -*-

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

(require 'markdown-mode)

(defvar-local pr-review--input-saved-window-config nil)
(defvar-local pr-review--input-exit-callback nil)
(defvar-local pr-review--input-allow-empty nil)
(defvar-local pr-review--input-refresh-after-exit nil)
(defvar-local pr-review--input-prev-marker nil)

(defvar pr-review-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'pr-review-input-exit)
    (define-key map "\C-c\C-k" 'pr-review-input-abort)
    map))

(define-minor-mode pr-review-input-mode
  "Minor mode for PR Review comment input buffer."
  :lighter " PrReviewCommentInput"
  :interactive nil
  (setq-local truncate-lines nil))

(defun pr-review-input-abort ()
  "Abort current comment input buffer, discard content."
  (interactive)
  (unless pr-review-input-mode (error "Invalid mode"))
  (let ((saved-window-config pr-review--input-saved-window-config))
    (kill-buffer)
    (when saved-window-config
      (unwind-protect
          (set-window-configuration saved-window-config)))))

(declare-function pr-review-refresh "pr-review")
(defun pr-review-input-exit ()
  "Apply content and exit current comment input buffer."
  (interactive)
  (unless pr-review-input-mode (error "Invalid mode"))
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (when (and pr-review--input-exit-callback
               (or pr-review--input-allow-empty
                   (not (string-empty-p content))))
      (funcall pr-review--input-exit-callback content)))
  (let ((refresh-after-exit pr-review--input-refresh-after-exit)
        (prev-marker pr-review--input-prev-marker))
    (pr-review-input-abort)
    (when refresh-after-exit
      (when-let ((prev-buffer (marker-buffer prev-marker))
                 (prev-pos (marker-position prev-marker)))
        (switch-to-buffer prev-buffer)
        (pr-review-refresh)
        (goto-char prev-pos)))))

(defun pr-review--open-input-buffer (description open-callback exit-callback &optional refresh-after-exit allow-empty)
  "Open a comment buffer for user input with DESCRIPTION,
OPEN-CALLBACK is called when the buffer is opened,
EXIT-CALLBACK is called when the buffer is exit (not abort),
both callbacks are called inside the comment buffer,
if REFRESH-AFTER-EXIT is not nil,
refresh the current pr-review buffer after exit."
  (let ((marker (point-marker)))
    (with-current-buffer (generate-new-buffer "*pr-review input*")
      (gfm-mode)
      (pr-review-input-mode)

      (setq-local
       header-line-format (concat description " "
                                  (substitute-command-keys
                                   (concat "Confirm with `\\[pr-review-input-exit]' or "
                                           "abort with `\\[pr-review-input-abort]'")))
       pr-review--input-saved-window-config (current-window-configuration)
       pr-review--input-exit-callback exit-callback
       pr-review--input-refresh-after-exit refresh-after-exit
       pr-review--input-prev-marker marker
       pr-review--input-allow-empty allow-empty)

      (when open-callback
        (funcall open-callback))

      (replace-string-in-region "\r\n" "\n" (point-min) (point-max))
      (switch-to-buffer-other-window (current-buffer)))))


(provide 'pr-review-input)
;;; pr-review-input.el ends here
