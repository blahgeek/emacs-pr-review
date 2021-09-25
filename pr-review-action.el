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
    (when (pr-review--review-thread-section-p section)
      (pr-review--open-input-buffer
       "Reply to thread." nil
       (apply-partially 'pr-review--post-review-comment-reply
                        pr-review--pr-node-id
                        (oref section top-comment-id))
       'refresh-after-exit))))

(defun pr-review-resolve-thread (&rest _)
  "Resolve or unresolve current thread."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (pr-review--review-thread-section-p section)
      (let ((resolved (oref section is-resolved))
            (thread-id (oref section value)))
        (when (y-or-n-p (format "Really %s this thread?"
                                (if resolved "unresolve" "resolve")))
          (pr-review--post-resolve-review-thread
           thread-id (not resolved))
          (pr-review-refresh))))))

(defun pr-review-comment (&rest _)
  "Post comment to this PR."
  (interactive)
  (let ((section (magit-current-section))
        reply-content)
    (when (pr-review-comment--section-p section)
      (save-excursion
        (goto-char (oref section start))
        (forward-line)  ;; skip section heading
        (setq reply-content (buffer-substring-no-properties
                             (point) (oref section end)))))
    (pr-review--open-input-buffer
     "Comment to PR."
     (lambda () (when reply-content (insert "> " reply-content)))
     (apply-partially 'pr-review--post-comment
                      pr-review--pr-node-id)
     'refresh-after-exit)))


(defun pr-review--get-diff-line-info (pt)
  "Return (side . (filename . line)) for diff line at PT."
  (save-excursion
    (goto-char pt)
    (beginning-of-line)
    (let (prop)
      (cond
       ((setq prop (get-text-property (point) 'pr-review-diff-line-left))
        (cons "LEFT" prop))
       ((setq prop (get-text-property (point) 'pr-review-diff-line-right))
        (cons "RIGHT" prop))))))


(defun pr-review--add-pending-review-thread-exit-callback (orig-buffer review-thread body)
  (setf (alist-get 'body review-thread) body)
  (when (buffer-live-p orig-buffer)
    (with-current-buffer orig-buffer
      (let ((inhibit-read-only t))
        (pr-review--insert-in-diff-pending-review-thread review-thread))
      (set-buffer-modified-p t)
      (push review-thread pr-review--pending-review-threads))))

(defun pr-review-add-pending-review-thread ()
  "Add pending review thread under current point (must be in a diff line).
When a region is active, the review thread is added for multiple lines."
  (interactive)
  (let* ((line-info (pr-review--get-diff-line-info
                     (if (use-region-p) (1- (region-end)) (point))))
         (start-line-info (when (use-region-p)
                            (pr-review--get-diff-line-info (region-beginning))))
         region-text
         review-thread)
    (if (or (null line-info)
            (and start-line-info
                 (not (equal (cadr line-info) (cadr start-line-info)))))
        (message "Cannot add review thread at current point")
      (setq review-thread `((path . ,(cadr line-info))
                            (line . ,(cddr line-info))
                            (side . ,(car line-info))
                            (startLine . ,(cddr start-line-info))
                            (startSide . ,(car start-line-info))))
      (when (use-region-p)
        (setq region-text (replace-regexp-in-string
                           (rx line-start (any ?+ ?- ?\s)) ""
                           (buffer-substring-no-properties (region-beginning) (region-end))))
        (unless (string-suffix-p "\n" region-text)
          (setq region-text (concat region-text "\n"))))
      (pr-review--open-input-buffer
       "Start review thread."
       (when region-text
         (lambda ()
           (insert "```suggestion\n" region-text "```")
           (beginning-of-buffer)))
       (apply-partially 'pr-review--add-pending-review-thread-exit-callback
                        (current-buffer)
                        review-thread))
      t)))

(defun pr-review-edit-pending-review-thread ()
  "Edit pending review thread under current point."
  (interactive)
  (when-let ((review-thread (get-text-property (point) 'pr-review-pending-review-thread))
             (beg (get-text-property (point) 'pr-review-pending-review-beg))
             (end (get-text-property (point) 'pr-review-pending-review-end)))
    (let ((inhibit-read-only t))
      (delete-region beg end))
    (setq-local pr-review--pending-review-threads
                (delq review-thread pr-review--pending-review-threads))
    (pr-review--open-input-buffer
     "Edit review thread."
     (lambda ()
       (insert (alist-get 'body review-thread))
       (beginning-of-buffer))
     (apply-partially 'pr-review--add-pending-review-thread-exit-callback
                      (current-buffer)
                      review-thread))
    t))

(defun pr-review-edit-or-add-pending-review-thread ()
  "Edit pending review thread or add a new one, depending on the current point."
  (interactive)
  (or (pr-review-edit-pending-review-thread)
      (pr-review-add-pending-review-thread)))

(defun pr-review--submit-review-exit-callback (orig-buffer event body)
  (when (buffer-live-p orig-buffer)
    (with-current-buffer orig-buffer
      (pr-review--post-review pr-review--pr-node-id
                              pr-review--head-commit-id
                              event
                              pr-review--pending-review-threads
                              body)
      (setq-local pr-review--pending-review-threads nil))))

(defun pr-review-submit-review (event)
  "Submit review with pending review threads, with action EVENT.
When called interactively, user will be asked to choose an event."
  (interactive (list (completing-read "Select review action: "
                                      '("COMMENT" "APPROVE" "REQUEST_CHANGES")
                                      nil 'require-match)))
  (pr-review--open-input-buffer
   (format "Submit review %s." event)
   nil
   (apply-partially 'pr-review--submit-review-exit-callback
                    (current-buffer) event)
   'refresh-after-exit))

(provide 'pr-review-action)
;;; pr-review-action.el ends here
