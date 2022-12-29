;;; indie-org-tests.el --- ERT tests for indie-org    -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Michael Herstine <sp1ff@pobox.com>

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

;; Unit tests for `indie-org.el'.

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'indie-org)
(require 'indie-org-rss)

(defvar srcdir (getenv "srcdir"))
(cl-assert srcdir t "Please specifiy the environment variable 'srcdir'.")

(defvar builddir (getenv "builddir"))
(cl-assert builddir t "Please specifiy the environment variable 'builddir'.")

(indie-org-enable)

(ert-deftest indie-org-test-rss ()
  "Test the production of RSS 2.0 feeds."
  (let* ((source-directory
          (concat
           (file-name-as-directory
            (expand-file-name srcdir))
           "test-posts/"))
         (base-directory
          (make-temp-file "indie-org-test-rss" t))
         (project
          (indie-org-rss-enable-rss-2.0
           (list
            "rss"
            :base-directory
            base-directory
            :publishing-directory
            base-directory)
           "rss.org"
           "Test Posts"
           "Test Description")))
    ;; Copy test files over
    (cl-loop for f in '("a.org" "b.org") do
         (copy-file
          (concat source-directory f)
          (concat base-directory f)))
    (org-publish-project project t)
    ;; I'd like to tighten-up these tests-- I just don't want to make
    ;; 'em too detailed until the test posts take shape.
    (should (file-exists-p (concat (file-name-as-directory base-directory) "rss.org")))
    (should (file-exists-p (concat (file-name-as-directory base-directory) "rss.xml")))))


(provide 'indie-org-tests)

;;; indie-org-tests.el ends here
