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

(require 'subr-x)
(require 'magit-section)
(require 'magit-diff)

(defgroup pr-review nil "Pr review.")

(defface pr-review-title-face
  '((t :inherit outline-1))
  "Face used for title."
  :group 'pr-review)

(defface pr-review-state-face
  '((t :inherit bold))
  "Face used for state (e.g. MERGED)."
  :group 'pr-review)

(defface pr-review-author-face
  '((t :inherit font-lock-keyword-face))
  "Face used for author names."
  :group 'pr-review)

(defface pr-review-timestamp-face
  '((t :slant italic))
  "Face used for timestamps."
  :group 'pr-review)

(defface pr-review-branch-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for branchs."
  :group 'pr-review)

(defface pr-review-label-face
  '((t :box t :foregroud "black"))
  "Face used for labels."
  :group 'pr-review)

(defface pr-review-thread-item-title-face
  '((t :inherit bold))
  "Face used for title of review thread item."
  :group 'pr-review)

(defface pr-review-thread-diff-begin-face
  '((t :underline t :extend t :inherit font-lock-comment-face))
  "Face used for the beginning of thread diff hunk."
  :group 'pr-review)

(defface pr-review-thread-diff-end-face
  '((t :overline t :extend t :inherit font-lock-comment-face))
  "Face used for the beginning of thread diff hunk."
  :group 'pr-review)

(defface pr-review-in-diff-thread-title-face
  '((t :inherit font-lock-comment-face))
  "Face used for the title of the in-diff thread title."
  :group 'pr-review)

(defface pr-review-in-diff-pending-begin-face
  '((t :underline t :extend t :inherit bold-italic))
  "Face used for start line of pending-thread in the diff."
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

;; keymaps

(defvar pr-review-review-thread-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pr-review-reply-to-thread)
    (define-key map (kbd "C-c C-s") #'pr-review-resolve-thread)
    map))

(defvar pr-review-review-thread-item-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'pr-review-edit-review-comment)
    (define-key map (kbd "C-c C-c") #'pr-review-reply-to-thread)
    (define-key map (kbd "C-c C-s") #'pr-review-resolve-thread)
    map))

(defvar pr-review-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pr-review-comment)
    (define-key map (kbd "C-c C-e") #'pr-review-edit-comment)
    map))

(defvar pr-review-review-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pr-review-comment)
    (define-key map (kbd "C-c C-e") #'pr-review-edit-review)
    map))

(defvar pr-review-diff-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") #'pr-review-edit-pending-review-thread)
    (define-key map (kbd "C-c C-c") #'pr-review-edit-or-add-pending-review-thread)
    (define-key map (kbd "C-c C-s") #'pr-review-submit-review)
    (define-key map (kbd "C-c C-v") #'pr-review-view-file)
    map))

(defvar-local pr-review--pr-path nil "List of repo-owner, repo-name, pr-id.")
(defvar-local pr-review--pr-node-id nil)
(defvar-local pr-review--head-commit-id nil)
(defvar-local pr-review--base-commit-id nil)
(defvar-local pr-review--pending-review-threads nil)

(provide 'pr-review-common)
;;; pr-review-common.el ends here
