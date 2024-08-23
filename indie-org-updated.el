;;; indie-org-updated.el --- indie-org support for :updated  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Michael Herstine <sp1ff@pobox.com>

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

;; None.

;;; Code:

(defun indie-org-updated-time (file project)
  "Return FILE's updated property as a Lisp timestamp.
Return nil if FILE has no such property.
PROJECT is the current project.

At the time of this writing, the only attribute of PROJECT actually used
is :base-directory."

  (let ((upd (org-publish-find-property file :updated project 'indie-org)))
    ;; If `upd' is nil, `org-element-interpret-data' will return "", and
    ;; `org-time-string-to-time' will error-out.
    (and upd (org-time-string-to-time (org-element-interpret-data upd)))))

(defun indie-org-updated-time-or-date (file project)
  "Return FILE's updated property, or date otherwise.
The date will be in the form of a Lisp timesetamp.
PROJECT is the current project."

  (or (indie-org-updated-time file project)
      (time-convert (org-publish-find-date file project) t)))

(defun indie-org-updated--check-updated (oldfun file project)
  "Return FILE's UPDATED time if available, retrieve DATE (via OLDFUN) otherwise.
PROJECT is the current project."
  (or (indie-org-updated-time file project) (funcall oldfun file project)))

(defun indie-org-updated-advise-find-date (_project)
  "Advise `org-publish-find-date' to return UPDATED, if available.

This function is suitable for use in :preparation-function."
  (advice-add #'org-publish-find-date :around #'indie-org-updated--check-updated))

(defun indie-org-updated-unadvise-find-date (_project)
  "Unadvise `org-publish-find-date' vis-a-vis `indie-org-updated-advise-find-date'.

This function is suitable for use in :completion-function."
  (advice-remove #'org-publish-find-date #'indie-org-updated--check-updated))

(provide 'indie-org-updated)

;;; indie-org-updated.el ends here.
