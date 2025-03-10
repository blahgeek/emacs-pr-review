;;; pr-review-common.el --- Common definitions for pr-review  -*- lexical-binding: t; -*-

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

(require 'magit-section)

(defgroup pr-review nil "Pr review."
  :group 'tools)

(defface pr-review-title-face
  '((t :inherit outline-1))
  "Face used for title."
  :group 'pr-review)

(defface pr-review-state-face
  '((t :inherit bold))
  "Face used for default state keywords."
  :group 'pr-review)

(defface pr-review-error-state-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face used for error state (e.g. changes requested)."
  :group 'pr-review)

(defface pr-review-success-state-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face used for success state (e.g. merged)."
  :group 'pr-review)

(defface pr-review-info-state-face
  '((t :slant italic))
  "Face used for info (unimportant) state (e.g. resolved)."
  :group 'pr-review)

(defface pr-review-author-face
  '((t :inherit font-lock-keyword-face))
  "Face used for author names."
  :group 'pr-review)

(defface pr-review-check-face
  '((t :inherit pr-review-author-face))
  "Face used for check names."
  :group 'pr-review)

(defface pr-review-timestamp-face
  '((t :slant italic))
  "Face used for timestamps."
  :group 'pr-review)

(defface pr-review-branch-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for branchs."
  :group 'pr-review)

(defface pr-review-hash-face
  '((t :inherit font-lock-comment-face))
  "Face used for commit hash."
  :group 'pr-review)

(defface pr-review-label-face
  '((t :box t :foregroud "black"))
  "Face used for labels."
  :group 'pr-review)

(defface pr-review-thread-item-title-face
  '((t :inherit magit-section-secondary-heading))
  "Face used for title of review thread item."
  :group 'pr-review)

(defface pr-review-thread-diff-begin-face
  '((t :underline t :extend t :inherit font-lock-comment-face))
  "Face used for the beginning of thread diff hunk."
  :group 'pr-review)

(defface pr-review-thread-diff-body-face
  '((t))
  "Extra face added to the body of thread diff hunk."
  :group 'pr-review)

(defface pr-review-thread-diff-end-face
  '((t :overline t :extend t :inherit font-lock-comment-face))
  "Face used for the beginning of thread diff hunk."
  :group 'pr-review)

(defface pr-review-thread-comment-face
  '((t))
  "Extra face added to review thread comments."
  :group 'pr-review)

(defface pr-review-in-diff-thread-title-face
  '((t :inherit font-lock-comment-face))
  "Face used for the title of the in-diff thread title."
  :group 'pr-review)

(defface pr-review-in-diff-pending-begin-face
  '((t :underline t :extend t :inherit bold-italic))
  "Face used for start line of pending-thread in the diff."
  :group 'pr-review)

(defface pr-review-in-diff-pending-body-face
  '((t))
  "Extra face added to the comment body of pending-thread in the diff."
  :group 'pr-review)

(defface pr-review-in-diff-pending-end-face
  '((t :overline t :extend t :height 0.5 :inherit bold-italic))
  "Face used for end line of pending-thread in the diff."
  :group 'pr-review)

(defface pr-review-link-face
  '((t :underline t))
  "Face used for links."
  :group 'pr-review)

(defface pr-review-button-face
  '((t :underline t :slant italic))
  "Face used for buttons."
  :group 'pr-review)

(defface pr-review-reaction-face
  '((t :height 0.7 :box t))
  "Face used for reaction emojis."
  :group 'pr-review)

(defface pr-review-fringe-comment-pending
  '((t :inherit warning))
  "Face used for fringe icons for pending comments.")

(defface pr-review-fringe-comment-open
  '((t :inherit font-lock-constant-face))
  "Face used for fringe icons for open comments.")

(defface pr-review-fringe-comment-resolved
  '((t :inherit shadow))
  "Face used for fringe icons for resolved comments.")

;; section classes
(defclass pr-review--review-section (magit-section)
  ((body :initform nil)
   (updatable :initform nil)
   (databaseId :initform nil)
   (reaction-groups :initform nil)))

(defclass pr-review--comment-section (magit-section)
  ((body :initform nil)
   (updatable :initform nil)
   (databaseId :initform nil)
   (reaction-groups :initform nil)))

(defclass pr-review--diff-section (magit-section) ())
(defclass pr-review--check-section (magit-section) ())
(defclass pr-review--commit-section (magit-section) ())

(defclass pr-review--review-thread-section (magit-section)
  ((top-comment-id :initform nil)
   (is-resolved :initform nil)))

(defclass pr-review--review-thread-item-section (magit-section)
  ((body :initform nil)
   (updatable :initform nil)
   (databaseId :initform nil)
   (reaction-groups :initform nil)))

(defclass pr-review--root-section (magit-section)
  ((title :initform nil)
   (updatable :initform nil)))

(defclass pr-review--description-section (magit-section)
  ((body :initform nil)
   (updatable :initform nil)
   (reaction-groups :initform nil)))

(defclass pr-review--event-section (magit-section) ())

(defvar-local pr-review--pr-path nil "List of repo-owner, repo-name, pr-id.")
(defvar-local pr-review--pr-info nil "Result of fetch-pr-info, useful for actions.")
(defvar-local pr-review--pending-review-threads nil)
(defvar-local pr-review--selected-commits nil)
(defvar-local pr-review--selected-commit-base nil)
(defvar-local pr-review--selected-commit-head nil)

(defcustom pr-review-generated-file-regexp ".*generated/.*"
  "Regexe that match generated files, which would be collapsed in review."
  :type 'regexp
  :group 'pr-review)

(defcustom pr-review-diff-font-lock-syntax 'hunk-also
  "This value is assigned to `diff-font-lock-syntax' to fontify hunk.
Set to nil to disable source language syntax highlighting."
  :type (get 'diff-font-lock-syntax 'custom-type)
  :group 'pr-review)

(defcustom pr-review-diff-hunk-limit 4
  "Maximum number of lines shown for diff hunks in review threads."
  :type 'number
  :group 'pr-review)

(defvar pr-review-reaction-emojis
  '(("CONFUSED" . "😕")
    ("EYES" . "👀")
    ("HEART" . "❤️")
    ("HOORAY" . "🎉")
    ("LAUGH" . "😄")
    ("ROCKET" . "🚀")
    ("THUMBS_DOWN" . "👎")
    ("THUMBS_UP" . "👍"))
  "Alist of github reaction name to emoji unicode.
See https://docs.github.com/en/graphql/reference/enums#reactioncontent")

(provide 'pr-review-common)
;;; pr-review-common.el ends here
