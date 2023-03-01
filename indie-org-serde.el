;;; indie-org-serde.el -- indie-org serialization/deserialization primatives  -*- lexical-binding: t -*-

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

;; Low-level serde primitives for `indie-org.

;;; Code:

(defun indie-org-serde-hash-to-plist (hash &rest kwargs)
  "Serialize a hash table HASH to a plist.
KWARGS is an optional set of keyword arguments.

Specify keyword argument `:serializer' to be used as the function
for serializing the hash table values (if not specified,
`identity' will be used)"

  (let ((plist)
        (serializer
         (or (plist-get kwargs :serializer) #'identity)))
    (maphash
     (lambda (key value)
       (setq plist (append plist (list key (funcall serializer value)))))
     hash)
    plist))

(defun indie-org-serde-plist-to-hash (plist &rest kwargs)
  "Deserialize PLIST into a hash table.
KWARGS is an optional set of keyword arguments.

Specify the `:test' to give a test for the hashtable (default is
`eql').  Specify the `:deserializer' argument as a function to be
used to deserialize values (`identity' will be used by default)."

  (let* ((test (or (plist-get kwargs :test) #'eql))
         (hash (make-hash-table :test test))
         (deserializer (or (plist-get kwargs :deserializer) #'identity)))
    (while plist
      (let ((key (car plist))
            (value (cadr plist)))
        (puthash key (funcall deserializer value) hash)
        (setq plist (cddr plist))))
    hash))

(provide 'indie-org-serde)
;;; indie-org-serde.el ends here.
