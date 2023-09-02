;;; indie-org-mf.el --- microformats support  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Michael Herstine <sp1ff@pobox.com>

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

(defun indie-org-mf-check-entity (entity ty props &optional check-props-exist)
  "Verify ENTITY has type TY & properties PROPS.
If CHECK-PROPS-EXIST is non-nil, PROPS shall be a list of properties to be
checked for existence.

ENTITY shall be a property list representing an mf2 entity, such
as that returned by `json-parse-string' with an :object-type
option of 'plist.

TY shall be the mf type ENTITY should have (e.g. \"h-card\").

PROPS shall be a list of alternating property keywords (:photo,
:note, &c) along with their expected values."

  (let ((entity-types (plist-get entity :type))
        (entity-props (plist-get entity :properties)))
    (unless (seq-find (lambda (x) (equal x ty)) entity-types)
      (error "Expected a %s, got %s" ty types))
    (while props
      (if check-props-exist
          (let* ((prop (car props))
                 (actual (plist-get entity-props prop)))
            (unless actual
              (error "Expected a property of %s, but it doesn't exist" prop))
            (setq props (cdr props)))
        (let* ((prop (car props))
               (val (cadr props))
               (actual (plist-get entity-props prop)))
          (unless (equal val actual)
            (error "Expected a value for %s of %s, got %s" prop val actual))
          (setq props (cddr props)))))))

(defun indie-org-mf-check-children (entity ty props &optional check-props-exist)
  "Verify the children of ENTITY are of type TY & have properties PROPS.
If CHECK-PROPS-EXIST is non-nil, PROPS shall be a list of properties to be
checked for existence."

  (let ((children (plist-get entity :children)))
    (seq-map
     (lambda (x) (indie-org-mf-check-entity x ty props check-props-exist))
     children)))

(provide 'indie-org-mf)
;;; indie-org-mf.el ends here.
