;;; pr-review-api.el --- API functions for pr-review  -*- lexical-binding: t; -*-

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
(require 'ghub)

(defcustom pr-review-ghub-auth-name 'emacs-pr-review
  "Ghub auth name used by pr-review, see `ghub-request' for details."
  :type 'symbol
  :group 'pr-review)

(defcustom pr-review-ghub-username nil
  "Ghub username used by pr-review, see `ghub-request' for details."
  :type '(choice (const :tag "Read from config" nil)
                 (string :tag "Username value"))
  :group 'pr-review)

(defvar pr-review--bin-dir (file-name-directory load-file-name))

(defun pr-review--get-graphql (name)
  "Get graphql content for NAME (symbol), cached."
  (with-temp-buffer
    (insert-file-contents-literally
     (concat pr-review--bin-dir "graphql/" (symbol-name name) ".graphql"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun pr-review--execute-graphql (name variables)
  (let ((res (ghub-graphql (pr-review--get-graphql name)
                           variables
                           :auth pr-review-ghub-auth-name
                           :username pr-review-ghub-username)))
    (let-alist res
      (when .errors
        (message "%s" res)
        (let-alist (car .errors)
          (error "Error while making graphql request %s: %s: %s"
                 name .type .message)))
      .data)))

(defun pr-review--fetch-pr-info ()
  (pcase-let ((`(,repo-owner ,repo-name ,pr-id) pr-review--pr-path))
    (let-alist (pr-review--execute-graphql
                'get-pull-request
                `((repo_owner . ,repo-owner)
                  (repo_name . ,repo-name)
                  (pr_id . ,(if (numberp pr-id)
                                pr-id
                              (string-to-number pr-id)))))
      .repository.pullRequest)))

(defun pr-review--fetch-compare (base-ref head-ref)
  (let* ((repo-owner (car pr-review--pr-path))
         (repo-name (cadr pr-review--pr-path))
         (res (ghub-request
               "GET"
               (format "/repos/%s/%s/compare/%s...%s"
                       repo-owner repo-name base-ref head-ref)
               '()
               :headers '(("Accept" . "application/vnd.github.v3.diff"))
               :reader 'ghub--decode-payload
               :auth pr-review-ghub-auth-name
               :username pr-review-ghub-username)))
    ;; magit-diff expects diff with --no-prefix
    (setq res (replace-regexp-in-string
               (rx line-start "diff --git a/" (group-n 1 (+? not-newline)) " b/" (backref 1) line-end)
               "diff --git \\1 \\1"
               res))
    (setq res (replace-regexp-in-string
               (rx line-start (group-n 1 (or "+++" "---")) " " (or "a/" "b/") (group-n 2 (+? not-newline)) line-end)
               "\\1 \\2"
               res))
    (concat res "\n")))  ;; don't why, just need an extra new line

(defvar-local pr-review--compare-cache-refs nil)
(defvar-local pr-review--compare-cache-result nil)
(defun pr-review--fetch-compare-cached (base-ref head-ref)
  (unless (and pr-review--compare-cache-result
               (equal pr-review--compare-cache-refs (cons base-ref head-ref)))
    (let ((res (pr-review--fetch-compare base-ref head-ref)))
      (setq-local pr-review--compare-cache-result res
                  pr-review--compare-cache-refs (cons base-ref head-ref))))
  pr-review--compare-cache-result)


(defun pr-review--fetch-file (filepath head-or-base)
  (let* ((repo-owner (car pr-review--pr-path))
         (repo-name (cadr pr-review--pr-path))
         (url (format "/repos/%s/%s/contents/%s" repo-owner repo-name filepath))
         (ref (pcase head-or-base
                ('head pr-review--head-commit-id)
                ('base pr-review--base-commit-id))))
    (ghub-request "GET" url `((ref . ,ref))
                  :headers '(("Accept" . "application/vnd.github.v3.raw"))
                  :reader 'ghub--decode-payload
                  :auth pr-review-ghub-auth-name
                  :username pr-review-ghub-username)))

(defun pr-review--post-review-comment-reply (pr-node-id top-comment-id body)
  (let (res review-id)
    (setq res (let-alist (pr-review--execute-graphql
                          'add-review-comment-reply
                          `((input . ((pullRequestId . ,pr-node-id)
                                      (inReplyTo . ,top-comment-id)
                                      (body . ,body)))))
                .addPullRequestReviewComment.comment))
    (unless (equal 1 (let-alist res (length .pullRequestReview.comments.nodes)))
      (error "Error while adding review comment reply, abort"))
    (setq review-id (let-alist res .pullRequestReview.id))
    (setq res (let-alist (pr-review--execute-graphql
                          'submit-review
                          `((input . ((pullRequestReviewId . ,review-id)
                                      (event . "COMMENT")))))
                .submitPullRequestReview.pullRequestReview))
    (unless (equal review-id (alist-get 'id res))
      (error "Error while submitting review comment reply"))))

(defun pr-review--post-comment (pr-node-id body)
  (pr-review--execute-graphql 'add-comment
                              `((input . ((subjectId . ,pr-node-id)
                                          (body . ,body))))))

(defun pr-review--update-comment (comment-id body)
  (pr-review--execute-graphql 'update-comment
                              `((input . ((id . ,comment-id)
                                          (body . ,body))))))

(defun pr-review--update-review (review-id body)
  (pr-review--execute-graphql 'update-review
                              `((input . ((pullRequestReviewId . ,review-id)
                                          (body . ,body))))))

(defun pr-review--update-review-comment (review-comment-id body)
  (pr-review--execute-graphql 'update-review-comment
                              `((input . ((pullRequestReviewCommentId . ,review-comment-id)
                                          (body . ,body))))))

(defun pr-review--update-pr-body (pr-node-id body)
  (pr-review--execute-graphql 'update-pr-body
                              `((input . ((pullRequestId . ,pr-node-id)
                                          (body . ,body))))))

(defun pr-review--post-resolve-review-thread (review-thread-id resolve-or-unresolve)
  (pr-review--execute-graphql (if resolve-or-unresolve
                                  'resolve-review-thread
                                'unresolve-review-thread)
                              `((input . ((threadId . ,review-thread-id))))))

(defun pr-review--post-review (pr-node-id commit-id event pending-threads body)
  (pr-review--execute-graphql
   'add-review
   `((input . ((body . ,body)
               (commitOID . ,commit-id)
               (event . ,event)
               (pullRequestId . ,pr-node-id)
               (threads . ,pending-threads))))))


(provide 'pr-review-api)
;;; pr-review-api.el ends here
