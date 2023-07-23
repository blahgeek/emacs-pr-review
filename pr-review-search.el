;;; pr-review-search.el --- Search PRs               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Yikai Zhao

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
(require 'pr-review-listview)
(require 'pr-review-api)

(declare-function pr-review-open "pr-review")

(define-derived-mode pr-review-search-mode pr-review-listview-mode
  "PrReviewSearch"
  :interactive nil
  :group 'pr-review

  (add-hook 'tabulated-list-revert-hook #'pr-review--search-refresh nil 'local)
  (setq-local pr-review--listview-open-callback #'pr-review--search-open-item
              tabulated-list-use-header-line nil
              tabulated-list-padding 2))

(defvar-local pr-review--search-query nil
  "The query string for searching.")

(defun pr-review--search-open-item (item)
  "Open the selected ITEM."
  (let-alist item
    (pr-review-open .repository.owner.login .repository.name .number)))

(defun pr-review--search-format-status (entry)
  "Format status for search item ENTRY."
  (let ((my-login (let-alist (pr-review--whoami-cached) .viewer.login))
        assigned review-requested commenters)
    (let-alist entry
      (setq assigned (cl-find-if (lambda (node) (equal my-login (let-alist node .login)))
                                 .assignees.nodes)
            review-requested (cl-find-if (lambda (node) (equal my-login (let-alist node .requestedReviewer.login)))
                                         .reviewRequests.nodes)))
    (dolist (participant-item (let-alist entry .participants.nodes))
      (let ((login (let-alist participant-item .login)))
        (unless (equal login my-login)
          (push login commenters))))
    (concat (let-alist entry
              (concat (propertize (downcase .state) 'face 'pr-review-listview-status-face) " "))
            (when assigned
              (propertize "assigned " 'face 'pr-review-listview-status-face))
            (when review-requested (propertize "review_requested " 'face 'pr-review-listview-status-face))
            (when commenters
              (mapconcat (lambda (s) (propertize (format "%s " s) 'face 'pr-review-listview-unimportant-activity-face))
                         (delete-dups (reverse commenters)) ""))
            )))

(defun pr-review--search-refresh ()
  "Refresh search buffer."
  (unless (eq major-mode 'pr-review-search-mode)
    (user-error "Not in search buffer"))

  (setq-local tabulated-list-format
              [("Opened" 12 nil)
               ("Author" 10 nil)
               ("Title" 85 nil)
               ("Status" 25 nil)])
  (let* ((all-items (pr-review--search-prs pr-review--search-query))
         (items (seq-filter (lambda (item) (equal (alist-get '__typename item) "PullRequest")) all-items)))
    (setq-local header-line-format
                (concat (format "Search results: %d. " (length items))
                        (unless (equal (length all-items) (length items))
                          (format "(%d non-PRs not displayed) " (- (length all-items) (length items))))
                        (propertize (format "Query: %s" pr-review--search-query)
                                    'face 'font-lock-comment-face)))
    (setq-local tabulated-list-entries
                (mapcar (lambda (item)
                          (let-alist item
                            (list item
                                  (vector
                                   (pr-review--listview-format-time .createdAt)
                                   .author.login
                                   (format "[%s] %s" .repository.nameWithOwner .title)
                                   (pr-review--search-format-status item)
                                   ))))
                        items))
    (tabulated-list-init-header)
    (message (format "Search result refreshed, %d items." (length items)))))

(defcustom pr-review-search-predefined-queries
  '(("is:pr archived:false author:@me is:open" . "Created")
    ("is:pr archived:false assignee:@me is:open" . "Assigned")
    ("is:pr archived:false mentions:@me is:open" . "Mentioned")
    ("is:pr archived:false review-requested:@me is:open" . "Review requests"))
  "Predefined queries for `pr-review-search'.  List of (query . name)."
  :type '(alist :key-type string :value-type string)
  :group 'pr-review)

(defcustom pr-review-search-default-query nil
  "Default query for `pr-review-search-open'."
  :type 'string
  :group 'pr-review)


(defun pr-review--search-read-query ()
  "Read query for search."
  (let ((completion-extra-properties
         (list :annotation-function
               (lambda (q) (concat " " (alist-get q pr-review-search-predefined-queries nil nil 'equal))))))
    (completing-read "Search GitHub> "
                     pr-review-search-predefined-queries
                     nil
                     nil  ;; no require-match
                     pr-review-search-default-query)))

;;;###autoload
(defun pr-review-search (query)
  "Search PRs using a custom QUERY and list result in buffer.
See github docs for syntax of QUERY.
When called interactively, you will be asked to enter the QUERY."
  (interactive (list (pr-review--search-read-query)))
  (with-current-buffer (get-buffer-create "*pr-review search*")
    (pr-review-search-mode)
    (setq-local pr-review--search-query query)
    (pr-review--search-refresh)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun pr-review-search-open (query)
  "Search PRs using a custom QUERY and open one of them.
See github docs for syntax of QUERY.
When called interactively, you will be asked to enter the QUERY."
  (interactive (list (pr-review--search-read-query)))
  (let* ((prs (pr-review--search-prs query))
         (prs-alist
          (mapcar
           (lambda (pr)
             (let-alist pr
               (cons (format "%s/%s: [%s] %s" .repository.nameWithOwner .number .state .title)
                     (list .repository.owner.login .repository.name .number))))
           prs))
         (selected-pr (completing-read "Select:" prs-alist nil 'require-match)))
    (when-let ((selected-value (alist-get selected-pr prs-alist nil nil 'equal)))
      (apply #'pr-review-open selected-value))))



(provide 'pr-review-search)
;;; pr-review-search.el ends here
