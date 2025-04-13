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

(defcustom pr-review-ghub-host nil
  "Ghub host used by `pr-review', see `ghub-request' for details."
  :type '(choice (const :tag "Read from config" nil)
                 (string :tag "Host value"))
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

(defun pr-review--execute-graphql-raw (query variables)
  "Execute graphql QUERY with VARIABLES, return result."
  (let ((res (apply #'ghub-graphql
                    query variables
                    (pr-review--ghub-common-request-args))))
    (let-alist res
      (unless .data
        (cond
         (.errors
          (let-alist (car .errors)
            (error "Error while making graphql request: %s: %s"
                   .type .message)))
         (.message
          (error "Error while making graphql request: %s"
                 .message))
         (t
          (error "Error while making graphql request"))))
      .data)))

(defun pr-review--execute-graphql (name variables)
  "Execute graphql from file NAME.graphql with VARIABLES, return result."
  (pr-review--execute-graphql-raw (pr-review--get-graphql name) variables))

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
HEAD-OR-BASE should be \='head or \='base, it determines the version to fetch."
  (let* ((repo-owner (car pr-review--pr-path))
         (repo-name (cadr pr-review--pr-path))
         (url (format "/repos/%s/%s/contents/%s" repo-owner repo-name filepath))
         (ref (let-alist pr-review--pr-info
                (pcase head-or-base
                  ('head (or pr-review--selected-commit-head .headRefOid))
                  ('base (or pr-review--selected-commit-base .baseRefOid))))))
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
  (let ((input `((body . ,body)
                 (commitOID . ,commit-id)
                 (event . ,event)
                 (pullRequestId . ,pr-node-id))))
    (when pending-threads
      (setq input (cons `(threads . ,(vconcat pending-threads)) input)))
    (pr-review--execute-graphql 'add-review
                                `((input . ,input)))))

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
  "Get assignable users for REPO-OWNER/REPO-NAME.
Return hashtable of login -> alist of \='id, \='login, \='name."
  (let ((has-next-page t)
        (res (make-hash-table :test 'equal))
        args
        cursor)
    (while has-next-page
      (setq args `((repo_owner . ,repo-owner)
                   (repo_name . ,repo-name)))
      (when cursor
        (setq args (cons `(cursor . ,cursor) args)))
      (let-alist (pr-review--execute-graphql 'get-assignable-users args)
        (mapc (lambda (usr) (puthash (alist-get 'login usr) usr res))
              .repository.assignableUsers.nodes)
        (setq has-next-page .repository.assignableUsers.pageInfo.hasNextPage
              cursor .repository.assignableUsers.pageInfo.endCursor)))
    res))

;; alist of (repo-owner . repo-name) -> users
(defvar pr-review--cached-assignable-users nil)

(defun pr-review--get-assignable-users ()
  "Get assignable users for current PR, cached.
See `pr-review--get-assignable-users-1' for return format."
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
                                          (userIds . ,(vconcat user-node-ids)))))))

(defun pr-review--post-subscription-update (pr-node-id state)
  "Send API request to update subscription to STATE for PR-NODE-ID."
  (pr-review--execute-graphql
   'update-subscription
   `((input . ((state . ,state)
               (subscribableId . ,pr-node-id))))))


(defvar pr-review--whoami-cache nil
  "Cache for `pr-review--whoami'.
\((host . username) . actualvalue), The cons is used for invalidating cache.")

(defun pr-review--whoami ()
  "Return current user info."
  (pr-review--execute-graphql 'whoami nil))

(defun pr-review--whoami-cached ()
  "Return current user info, cached."
  (if (equal (car pr-review--whoami-cache) (cons pr-review-ghub-host pr-review-ghub-username))
      (cdr pr-review--whoami-cache)
    (let ((res (pr-review--whoami)))
      (setq pr-review--whoami-cache
            (cons (cons pr-review-ghub-host pr-review-ghub-username)
                  res))
      res)))

(defun pr-review--batch-get-pr-info-for-notifications (prs)
  "Batch get PR info for notifications.
PRS should be a list of (id repo-owner repo-name pr-number unread-since).
unread-since is a ISO-8601 encoded UTC date string.
Return list of (id . response)"
  (message "Batching getting PR info for %d notifications..." (length prs))
  (let* ((single-query-template (pr-review--get-graphql 'get-pull-request-for-notification.inc))
         (query (concat "query {"
                        (mapconcat (lambda (pr)
                                     (concat
                                      (format "n%s: " (car pr))
                                      (apply #'format single-query-template (cdr pr))))
                                   prs "\n")
                        "}"))
         (raw-resp (pr-review--execute-graphql-raw query nil)))
    (mapcar (lambda (item) (cons (substring (symbol-name (car item)) 1)
                                 (cdadr item)))
            raw-resp)))

(defvar-local pr-review--notifications-pr-info-cache nil
  "Cache of PR infos for notifications.
A hashtable, key is the notification ID (string),
value is (last_updated . pr_info).
last_updated is the from the notification.")

(defun pr-review--notifications-pr-info-cache-remove (id)
  "Remove ID from `pr-review--notifications-pr-info-cache'."
  (remhash id pr-review--notifications-pr-info-cache))

(defvar pr-review--get-notifications-per-page 50)

(defun pr-review--get-notifications (include-read page)
  "Get a list of notifications.
If INCLUDE-READ is not nil, all notifications are returned,
PAGE is the number of pages of the notifications, start from 1."
  (let ((res (apply #'ghub-request
                    "GET" "/notifications"
                    `((all . ,(if include-read "true" "false"))
                      (per_page . ,(number-to-string pr-review--get-notifications-per-page))
                      (page . ,(number-to-string page)))
                    (pr-review--ghub-common-request-args))))
    (when (let-alist res (and .status (string-match-p "^[45]" .status)))
      (error "Error while getting notifications: %s" res))
    res))

(defun pr-review--mark-notification-read (id)
  "Mark notification ID as read."
  (pr-review--notifications-pr-info-cache-remove id)  ;; otherwise its pr-info would not be refreshed
  (apply #'ghub-request
         "PATCH" (format "/notifications/threads/%s" id)
         '()
         (pr-review--ghub-common-request-args)))

(defun pr-review--delete-notification (id)
  "Delete notification ID."
  (pr-review--notifications-pr-info-cache-remove id)  ;; otherwise its pr-info would not be refreshed
  (apply #'ghub-request
         "DELETE" (format "/notifications/threads/%s/subscription" id)
         '()
         (pr-review--ghub-common-request-args)))

(defun pr-review--get-notifications-with-extra-pr-info (&rest args)
  "Like `pr-review--get-notifications' with ARGS, but with extra PR info.
The PR info would be cached if possible."
  (unless pr-review--notifications-pr-info-cache
    (setq-local pr-review--notifications-pr-info-cache (make-hash-table :test 'equal)))
  (let* ((notifications (apply #'pr-review--get-notifications args))
         ;; only query those not in cache, or "updated_at" is updated
         (items-needs-query (seq-filter
                             (lambda (item)
                               (let-alist item
                                 (and
                                  (equal .subject.type "PullRequest")
                                  (not (equal .updated_at
                                              (car (gethash .id pr-review--notifications-pr-info-cache)))))))
                             notifications))
         (id-to-last-updated (make-hash-table :test 'equal))
         batch-query-result)
    (when items-needs-query
      (setq batch-query-result
            (pr-review--batch-get-pr-info-for-notifications
             (mapcar (lambda (item)
                       (let-alist item
                         (list .id .repository.owner.login .repository.name
                               (if (string-match (rx (group (+ (any digit))) eos) .subject.url)
                                   (string-to-number (match-string 1 .subject.url))
                                 (error "Invalid PR url %s" .subject.url))
                               (or .last_read_at "1970-01-01T00:00:00Z"))))
                     items-needs-query)))
      (dolist (item items-needs-query)
        (puthash (alist-get 'id item) (alist-get 'updated_at item) id-to-last-updated))
      (dolist (query-result batch-query-result)
        (puthash (car query-result)
                 (cons (gethash (car query-result) id-to-last-updated)
                       (cdr query-result))
                 pr-review--notifications-pr-info-cache)))
    ;; add 'pr-info to each item
    (mapcar (lambda (item)
              (cons (cons 'pr-info (cdr (gethash (alist-get 'id item) pr-review--notifications-pr-info-cache)))
                    item))
            notifications)))


(defun pr-review--get-repo-labels-1 (repo-owner repo-name)
  "Get labels for repo REPO-OWNER/REPO-NAME.
Return hashtable, name -> alist, which constists of at least
\='node_id, \='description, \='color."
  (let ((items (apply #'ghub-request
                     "GET" (format "/repos/%s/%s/labels" repo-owner repo-name)
                     '((per_page . "100"))
                     :unpaginate t
                     (pr-review--ghub-common-request-args)))
        (result (make-hash-table :test 'equal)))
    (dolist (item items)
      (let-alist item
        (when .name
          (puthash .name item result))))
    result))

;; alist of (repo-owner . repo-name) -> labels
(defvar pr-review--cached-repo-labels nil)

(defun pr-review--get-repo-labels ()
  "Get labels for current repo, cached.
See `pr-review--get-repo-labels-1' for return value."
  (let ((repo-owner (car pr-review--pr-path))
        (repo-name (cadr pr-review--pr-path)))
    (if-let ((res (alist-get (cons repo-owner repo-name)
                             pr-review--cached-repo-labels nil nil 'equal)))
        res
      (message "Fetching labels for %s/%s..." repo-owner repo-name)
      (setq res (pr-review--get-repo-labels-1 repo-owner repo-name))
      (setf (alist-get (cons repo-owner repo-name)
                       pr-review--cached-repo-labels nil nil 'equal)
            res)
      res)))

(defun pr-review--clear-labels (pr-node-id)
  "Clear labels for pull-request PR-NODE-ID."
  (pr-review--execute-graphql
   'clear-labels
   `((input . ((labelableId . ,pr-node-id))))))

(defun pr-review--add-labels (pr-node-id label-node-ids)
  "Add labels LABEL-NODE-IDS to pull-request PR-NODE-ID."
  (pr-review--execute-graphql
   'add-labels
   `((input . ((labelableId . ,pr-node-id)
               (labelIds . ,(vconcat label-node-ids)))))))

(defun pr-review--update-reactions (subject-id reactions)
  "Update REACTIONS to SUBJECT-ID.
REACTIONS is a list of reaction names.
Those not in the list would be removed."
  (let ((graphql (mapconcat
                  (lambda (enum-item)
                    (if (member (car enum-item) reactions)
                        (format "_%s: addReaction(input: { content: %s, subjectId: \"%s\" }) {clientMutationId}"
                                (car enum-item) (car enum-item) subject-id)
                      (format "_%s: removeReaction(input: { content: %s, subjectId: \"%s\" }) {clientMutationId}"
                              (car enum-item) (car enum-item) subject-id)))
                  pr-review-reaction-emojis
                  "\n")))
    (pr-review--execute-graphql-raw
     (concat "mutation { \n" graphql "\n}")
     nil)))

(provide 'pr-review-api)
;;; pr-review-api.el ends here
