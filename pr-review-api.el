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
  "Ghub auth name used by `pr-review', see `ghub-request' for details."
  :type 'symbol
  :group 'pr-review)

(defcustom pr-review-ghub-username nil
  "Ghub username used by `pr-review', see `ghub-request' for details."
  :type '(choice (const :tag "Read from config" nil)
                 (string :tag "Username value"))
  :group 'pr-review)

(defcustom pr-review-ghub-host ghub-default-host
  "Ghub host used by `pr-review', useful for enterprise github instances."
  :type 'string
  :group 'pr-review)

(defvar pr-review--bin-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun pr-review--get-graphql (name)
  "Get graphql content for NAME (symbol), cached."
  (with-temp-buffer
    (insert-file-contents-literally
     (concat pr-review--bin-dir "graphql/" (symbol-name name) ".graphql"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun pr-review--ghub-common-request-args ()
  "Return common args for `ghub-request' and `ghub-graphql'."
  (list :auth pr-review-ghub-auth-name
        :username pr-review-ghub-username
        :host pr-review-ghub-host))

(defun pr-review--execute-graphql (name variables)
  "Execute graphql from file NAME.graphql with VARIABLES, return result."
  (let ((res (apply #'ghub-graphql
                    (pr-review--get-graphql name) variables
                    (pr-review--ghub-common-request-args))))
    (let-alist res
      (when .errors
        (message "%s" res)
        (let-alist (car .errors)
          (error "Error while making graphql request %s: %s: %s"
                 name .type .message)))
      .data)))

(defun pr-review--fetch-pr-info ()
  "Fetch pr info based on current buffer's local variable."
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
  "Fetch git diff from BASE-REF to HEAD-REF for current buffer.
Also fix the result so that it looks like result of git diff --no-prefix."
  (when-let* ((repo-owner (car pr-review--pr-path))
              (repo-name (cadr pr-review--pr-path))
              ;; res may be nil (if the ref is deleted)
              ;; in which case we will return nil
              (res (apply #'ghub-request
                    "GET"
                    (format "/repos/%s/%s/compare/%s...%s"
                            repo-owner repo-name base-ref head-ref)
                    '()
                    :headers '(("Accept" . "application/vnd.github.v3.diff"))
                    :reader 'ghub--decode-payload
                    (pr-review--ghub-common-request-args))))
    ;; magit-diff expects diff with --no-prefix
    (setq res (replace-regexp-in-string
               (rx line-start "diff --git a/" (group-n 1 (+? not-newline)) " b/" (backref 1) line-end)
               "diff --git \\1 \\1"
               res))
    (setq res (replace-regexp-in-string
               (rx line-start (group-n 1 (or "+++" "---")) " " (or "a/" "b/") (group-n 2 (+? not-newline)) line-end)
               "\\1 \\2"
               res))
    res))

(defvar-local pr-review--compare-cache-refs nil)
(defvar-local pr-review--compare-cache-result nil)
(defun pr-review--fetch-compare-cached (base-ref head-ref)
  "Fetch git diff from BASE-REF to HEAD-REF.
Same as `pr-review--fetch-compare', but cached in buffer variable."
  (unless (and pr-review--compare-cache-result
               (equal pr-review--compare-cache-refs (cons base-ref head-ref)))
    (when-let ((res (pr-review--fetch-compare base-ref head-ref)))
      (setq-local pr-review--compare-cache-result res
                  pr-review--compare-cache-refs (cons base-ref head-ref))))
  pr-review--compare-cache-result)


(defun pr-review--fetch-file (filepath head-or-base)
  "Fetch file content for FILEPATH for current review buffer.
If HEAD-OR-BASE is t, fetch the head version; otherwise base version."
  (let* ((repo-owner (car pr-review--pr-path))
         (repo-name (cadr pr-review--pr-path))
         (url (format "/repos/%s/%s/contents/%s" repo-owner repo-name filepath))
         (ref (alist-get (pcase head-or-base
                           ('head 'headRefOid)
                           ('base 'baseRefOid))
                         pr-review--pr-info)))
    (apply #'ghub-request
           "GET" url `((ref . ,ref))
           :headers '(("Accept" . "application/vnd.github.v3.raw"))
           :reader 'ghub--decode-payload
           (pr-review--ghub-common-request-args))))

(defun pr-review--post-review-comment-reply (pr-node-id top-comment-id body)
  "Post review commit reply BODY to TOP-COMMENT-ID at PR-NODE-ID."
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
  "Post comment BODY at pr PR-NODE-ID."
  (pr-review--execute-graphql 'add-comment
                              `((input . ((subjectId . ,pr-node-id)
                                          (body . ,body))))))

(defun pr-review--update-comment (comment-id body)
  "Update comment to BODY for COMMENT-ID."
  (pr-review--execute-graphql 'update-comment
                              `((input . ((id . ,comment-id)
                                          (body . ,body))))))

(defun pr-review--update-review (review-id body)
  "Update review to BODY for REVIEW-ID."
  (pr-review--execute-graphql 'update-review
                              `((input . ((pullRequestReviewId . ,review-id)
                                          (body . ,body))))))

(defun pr-review--update-review-comment (review-comment-id body)
  "Update review comment to BODY for REVIEW-COMMENT-ID."
  (pr-review--execute-graphql 'update-review-comment
                              `((input . ((pullRequestReviewCommentId . ,review-comment-id)
                                          (body . ,body))))))

(defun pr-review--update-pr-body (pr-node-id body)
  "Update pr description to BODY for PR-NODE-ID."
  (pr-review--execute-graphql 'update-pr
                              `((input . ((pullRequestId . ,pr-node-id)
                                          (body . ,body))))))

(defun pr-review--update-pr-title (pr-node-id title)
  "Update pr title to TITLE for PR-NODE-ID."
  (pr-review--execute-graphql 'update-pr
                              `((input . ((pullRequestId . ,pr-node-id)
                                          (title . ,title))))))

(defun pr-review--post-resolve-review-thread (review-thread-id resolve-or-unresolve)
  "Resolve or unresolve review thread REVIEW-THREAD-ID.
If RESOLVE-OR-UNRESOLVE is non-nil, do resolve; otherwise do unresolve."
  (pr-review--execute-graphql (if resolve-or-unresolve
                                  'resolve-review-thread
                                'unresolve-review-thread)
                              `((input . ((threadId . ,review-thread-id))))))

(defun pr-review--post-review (pr-node-id commit-id event pending-threads body)
  "Post review to PR-NODE-ID with commit COMMIT-ID.
EVENT: review action, e.g. APPROVE;
PENDING-THREADS: inline review threads;
BODY: review comment body."
  (pr-review--execute-graphql
   'add-review
   `((input . ((body . ,body)
               (commitOID . ,commit-id)
               (event . ,event)
               (pullRequestId . ,pr-node-id)
               (threads . ,pending-threads))))))

(defun pr-review--post-merge-pr (pr-node-id method)
  "Send API request to merge pr PR-NODE-ID with METHOD."
  (pr-review--execute-graphql
   'merge-pr
   `((input . ((pullRequestId . ,pr-node-id)
               (mergeMethod . ,method))))))

(defun pr-review--post-close-pr (pr-node-id)
  "Send API request to close pr PR-NODE-ID."
  (pr-review--execute-graphql
   'close-pr
   `((input . ((pullRequestId . ,pr-node-id))))))

(defun pr-review--post-reopen-pr (pr-node-id)
  "Send API request to re-open pr PR-NODE-ID."
  (pr-review--execute-graphql
   'reopen-pr
   `((input . ((pullRequestId . ,pr-node-id))))))

(defun pr-review--search-prs (query)
  "Search pull requests with QUERY."
  (let-alist (pr-review--execute-graphql
              'search-prs
              `((query . ,query)))
    .search.nodes))

(defun pr-review--get-assignable-users-1 (repo-owner repo-name)
  "Get a list of assignable users for REPO-OWNER/REPO-NAME."
  (let ((has-next-page t)
        cursor res)
    (while has-next-page
      (let-alist (pr-review--execute-graphql 'get-assignable-users
                                             `((repo_owner . ,repo-owner)
                                               (repo_name . ,repo-name)
                                               (cursor . ,cursor)))
        (setq has-next-page .repository.assignableUsers.pageInfo.hasNextPage
              cursor .repository.assignableUsers.pageInfo.endCursor
              res (append res .repository.assignableUsers.nodes))))
    res))

;; alist of (repo-owner . repo-name) -> users
(defvar pr-review--cached-assignable-users nil)

(defun pr-review--get-assignable-users ()
  "Get a list of assignable users (alist of 'id, 'login, 'name)
for current PR, cached."
  (let ((repo-owner (car pr-review--pr-path))
        (repo-name (cadr pr-review--pr-path)))
    (if-let ((res (alist-get (cons repo-owner repo-name)
                             pr-review--cached-assignable-users nil nil 'equal)))
        res
      (message "Fetching assignable users for %s/%s..." repo-owner repo-name)
      (setq res (pr-review--get-assignable-users-1 repo-owner repo-name))
      (setf (alist-get (cons repo-owner repo-name)
                       pr-review--cached-assignable-users nil nil 'equal)
            res)
      res)))

(defun pr-review--post-request-reviews (pr-node-id user-node-ids)
  "Request review from USER-NODE-IDS for PR-NODE-ID."
  (pr-review--execute-graphql 'request-reviews
                              `((input . ((pullRequestId . ,pr-node-id)
                                          (userIds . ,user-node-ids))))))

(defvar pr-review--get-notifications-per-page 50)

(defun pr-review--get-notifications (include-read page)
  "Get a list of notifications.
If INCLUDE-READ is not nil, all notifications are returned,
PAGE is the number of pages of the notifications, start from 1."
  (apply #'ghub-request
         "GET" "/notifications"
         `((all . ,(if include-read "true" "false"))
           (per_page . ,(number-to-string pr-review--get-notifications-per-page))
           (page . ,(number-to-string page)))
         (pr-review--ghub-common-request-args)))

(defun pr-review--mark-notification-read (id)
  "Mark notification ID as read."
  (apply #'ghub-request
         "PATCH" (format "/notifications/threads/%s" id)
         '()
         (pr-review--ghub-common-request-args)))

(provide 'pr-review-api)
;;; pr-review-api.el ends here
