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

(require 'ghub)

(defvar pr-review-ghub-auth-name 'emacs-pr-review)
(defvar pr-review-ghub-username nil)
(defvar pr-review-bin-dir (file-name-directory load-file-name))

(defvar pr-review--graphql-cache '())

(defun pr-review--get-graphql (name)
  "Get graphql content for NAME (symbol), cached."
  (or (alist-get name pr-review--graphql-cache)
      (let (content)
        (with-temp-buffer
          (insert-file-contents-literally
           (concat pr-review-bin-dir "graphql/" (symbol-name name) ".graphql"))
          (setq content (buffer-string)))
        (push (cons name content) pr-review--graphql-cache)
        content)))

(defun pr-review--execute-graphql (name variables)
  (let ((res (ghub-graphql (pr-review--get-graphql name)
                           variables
                           :auth pr-review-ghub-auth-name
                           :username pr-review-ghub-username)))
    (let-alist res
      (when .errors
        (message "%s" res)
        (error "Error while making graphql request %s: %s: %s"
               name .errors.type .errors.message))
      .data)))

(defun pr-review--fetch-pr-info (repo-owner repo-name pr-id)
  (let-alist (pr-review--execute-graphql
              'get-pull-request
              `((repo_owner . ,repo-owner)
                (repo_name . ,repo-name)
                (pr_id . ,(if (numberp pr-id)
                              pr-id
                            (string-to-number pr-id)))))
    .repository.pullRequest))

(defun pr-review--fetch-compare (repo-owner repo-name base-ref head-ref)
  (let ((res (ghub-request
              "GET"
              (format "/repos/%s/%s/compare/%s...%s"
                      repo-owner repo-name base-ref head-ref)
              '()
              :headers '(("Accept" . "application/vnd.github.v3.diff"))
              :reader 'ghub--decode-payload
              :auth pr-review-ghub-auth-name
              :username pr-review-ghub-username)))
    (concat res "\n")))  ;; don't why, just need an extra new line

(defvar-local pr-review--compare-refs nil)
(defvar-local pr-review--compare-result nil)
(defun pr-review--fetch-compare-cached (repo-owner repo-name base-ref head-ref)
  (unless (and pr-review--compare-result
               (equal pr-review--compare-refs (cons base-ref head-ref)))
    (let ((res (pr-review--fetch-compare repo-owner repo-name base-ref head-ref)))
      (setq-local pr-review--compare-result res
                  pr-review--compare-refs (cons base-ref head-ref))))
  pr-review--compare-result)

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
