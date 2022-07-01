;;; test-pub-state.el --- ERT tests for indie-org publication state    -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Michael Herstine <sp1ff@pobox.com>

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

(defvar srcdir (getenv "srcdir"))
(cl-assert srcdir t "Please specifiy the environment variable 'srcdir'.")

(defvar builddir (getenv "builddir"))
(cl-assert builddir t "Please specifiy the environment variable 'builddir'.")

(defun hash-equal (hash1 hash2)
  "Compare two hash tables to see whether they are equal."
  (and (= (hash-table-count hash1)
          (hash-table-count hash2))
       (catch 'flag
         (maphash
          (lambda (x y)
            (unless (or (and (hash-table-p y) (gethash x hash2))
                        (equal (gethash x hash2) y))
              (message "%s != %s" (gethash x hash2) y)
              (throw 'flag nil)))
          hash1)
         (throw 'flag t))))

(indie-org-enable)

(ert-deftest test-pub-state-smoke ()
  "Smoke tests for indie-org publication state."
  (let ((state (indie-org-make-publication-state))
        (now (current-time))
        (state-file (make-temp-file "indie-org-test-")))
    (indie-org-update-last-published state now)
    (indie-org-update-last-published state now :stg)
    (indie-org-update-last-published state now :s3)
    (indie-org-write-publication-state state state-file)
    (let ((new-state (indie-org-read-publication-state state-file)))
      (should
       (equal
        (indie-org-get-last-published state :prod)
        (indie-org-get-last-published new-state :prod)))
      (should
       (equal
        (indie-org-get-last-published state :stg)
        (indie-org-get-last-published new-state :stg)))
      (should
       (equal
        (indie-org-get-last-published state :s3)
        (indie-org-get-last-published new-state :s3))))))

(ert-deftest test-pub-state-mentions-sent ()
  "Smoke test for webmentions sent."
  (let ((state (indie-org-make-publication-state))
        (now (current-time))
        (state-file (make-temp-file "indie-org-test-"))
        (hash (make-hash-table :test 'equal)))
    ;; Record two webmentions on index.html...
    (indie-org-update-webmentions-made "index.html" now '("https://foo.com" "https://bar.com") hash)
    (plist-put state :webmentions-sent (list :prod hash))
    (should (eq 1 (hash-table-count hash)))

    (let* ((sent (make-hash-table :test 'equal))
           (to-send (indie-org-required-webmentions hash sent))
           (now (current-time))
           (state (indie-org-make-publication-state))
           (state-file (make-temp-file "indie-org-test-")))
      (should (eq 2 (length to-send)))
      (indie-org-record-sent-webmention (car to-send) now sent)
      (setq to-send (indie-org-required-webmentions hash sent))
      (should (eq 1 (length to-send)))
      (indie-org-record-sent-webmention (car to-send) now sent)
      (setq to-send (indie-org-required-webmentions hash sent))
      (should (eq 0 (length to-send)))

      (indie-org-update-last-published state now)
      (setq state (indie-org-set-webmentions-made state hash))
      (setq state (indie-org-set-webmentions-sent state sent))
      (indie-org-write-publication-state state state-file)
      (let* ((new-state (indie-org-read-publication-state state-file))
             (old-made (indie-org-get-webmentions-made state))
             (new-made (indie-org-get-webmentions-made new-state)))
        (should
         (hash-equal
          old-made
          new-made))))))

(provide 'indie-org-tests)

;;; indie-org-tests.el ends here
