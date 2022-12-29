;;; indie-org-pub-tests.el --- ERT tests for indie-org-pub    -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>

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

;; Unit tests for `indie-org-pub.el'.

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'indie-org-pub)

(defvar srcdir (getenv "srcdir"))
(cl-assert srcdir t "Please specifiy the environment variable 'srcdir'.")

(defvar builddir (getenv "builddir"))
(cl-assert builddir t "Please specifiy the environment variable 'builddir'.")

(indie-org-pub-enable)

(ert-deftest indie-org-pub-test-find-posts ()
  "Test `indie-org-find-posts'."
  (let* ((project-dir
          (concat (file-name-as-directory (expand-file-name srcdir)) "test-posts"))
         (production-posts
          (indie-org-pub-find-posts project-dir t))
         (staging-posts
          (indie-org-pub-find-posts project-dir nil :exclude '("rss.org"))))
    ;; I'd like to improve these tests-- I just don't want to make 'em
    ;; too detailed until the test posts take shape.
    (should (eq (length production-posts) 1))
    (should (eq (length staging-posts) 2))))

(provide 'indie-org-pub-tests)
;;; indie-org-pub-tests.el ends here.
