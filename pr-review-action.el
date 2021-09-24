;;; pr-review-action.el --- Action part for pr-review  -*- lexical-binding: t; -*-

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
(require 'pr-review-input)
(require 'pr-review-render)


(defun pr-review-reply-to-thread (&rest _)
  "Reply to current thread."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (pr-review-review-thread-section-p section)
      (pr-review--open-comment-input-buffer
       "Reply to thread." nil
       (apply-partially 'pr-review--post-review-comment-reply
                        pr-review--pr-node-id
                        (oref section top-comment-id))
       'refresh-after-exit))))

(defun pr-review-resolve-thread (&rest _)
  "Resolve or unresolve current thread."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (pr-review-review-thread-section-p section)
      (let ((resolved (oref section is-resolved))
            (thread-id (oref section value)))
        (when (y-or-n-p (format "Really %s this thread?"
                                (if resolved "unresolve" "resolve")))
          (pr-review--post-resolve-review-thread
           thread-id (not resolved)))))))

(defun pr-review-comment (&rest _)
  "Post comment to this PR."
  (interactive)
  (let ((section (magit-current-section))
        reply-content)
    (when (pr-review-comment-section-p section)
      (save-excursion
        (goto-char (oref section start))
        (forward-line)  ;; skip section heading
        (setq reply-content (buffer-substring-no-properties
                             (point) (oref section end)))))
    (pr-review--open-comment-input-buffer
     "Comment to PR."
     (lambda () (when reply-content (insert "> " reply-content)))
     (apply-partially 'pr-review--post-comment
                      pr-review--pr-node-id)
     'refresh-after-exit)))



(provide 'pr-review-action)
;;; pr-review-action.el ends here
