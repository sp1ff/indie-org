;;; indie-org-tests.el --- ERT tests for indie-org    -*- lexical-binding: t; -*-

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

(indie-org-enable)

(ert-deftest indie-org-test-find-posts ()
  "Test `indie-org-find-posts'."
  (let* ((project-dir
          (concat (file-name-as-directory (expand-file-name srcdir)) "test-posts"))
         (production-posts
          (indie-org-find-posts project-dir t))
         (staging-posts
          (indie-org-find-posts project-dir nil :exclude '("rss.org"))))
    ;; I'd like to improve these tests-- I just don't want to make 'em
    ;; too detailed until the test posts take shape.
    (should (eq (length production-posts) 1))
    (should (eq (length staging-posts) 2))))

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
          (indie-org-enable-rss-2.0-feed
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

(cl-defun mock-request-call-1 (_url &rest settings
                                    &key
                                    (_params nil)
                                    (_data nil)
                                    (_headers nil)
                                    (_encoding 'utf-8)
                                    (_error nil)
                                    (_sync nil)
                                    (_response (make-request-response))
                                    &allow-other-keys)
  (let ((success (plist-get settings :success))
        (rsp (json-parse-string "{\"type\":\"feed\",\"name\":\"Webmentions\",\"children\":[{\"type\":\"entry\",\"author\":{\"type\":\"card\",\"name\":\"my test site\",\"photo\":\"\",\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/\"},\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-3.html\",\"published\":\"2022-06-05T10:49:00\",\"wm-received\":\"2022-06-05T22:35:37Z\",\"wm-id\":1410286,\"wm-source\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-3.html\",\"wm-target\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"name\":\"Indiweb Markup\",\"content\":{\"html\":\"<p><a href=\\\"https://www.unwoundstack.com\\\">unwoundstack</a> is now <a href=\\\"https://www.unwoundstack.com/blog/indieweb-markup.html\\\" class=\\\"u-in-reply-to\\\">using</a> microformats-- huzzah!</p>\",\"text\":\"unwoundstack is now using microformats-- huzzah!\"},\"in-reply-to\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"wm-property\":\"in-reply-to\",\"wm-private\":false},{\"type\":\"entry\",\"author\":{\"type\":\"card\",\"name\":\"my test site\",\"photo\":\"\",\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/\"},\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-2.html\",\"published\":\"2022-06-05T10:49:00\",\"wm-received\":\"2022-06-05T17:52:23Z\",\"wm-id\":1410220,\"wm-source\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-2.html\",\"wm-target\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"name\":\"Indiweb Markup\",\"content\":{\"html\":\"<p><a href=\\\"https://www.unwoundstack.com\\\">unwoundstack</a> is now <a href=\\\"https://www.unwoundstack.com/blog/indieweb-markup.html\\\">using</a> microformats-- huzzah!</p>\",\"text\":\"unwoundstack is now using microformats-- huzzah!\"},\"mention-of\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"wm-property\":\"mention-of\",\"wm-private\":false},{\"type\":\"entry\",\"author\":{\"type\":\"card\",\"name\":\"\",\"photo\":\"\",\"url\":\"\"},\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-1.html\",\"published\":null,\"wm-received\":\"2022-06-05T14:39:13Z\",\"wm-id\":1410172,\"wm-source\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-1.html\",\"wm-target\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"name\":\"Indiweb Markup\",\"content\":{\"html\":\"<p><a href=\\\"https://www.unwoundstack.com\\\">unwoundstack</a> is now <a href=\\\"https://www.unwoundstack.com/blog/indieweb-markup.html\\\">using</a> microformats-- huzzah!</p>\",\"text\":\"unwoundstack is now using microformats-- huzzah!\"},\"mention-of\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"wm-property\":\"mention-of\",\"wm-private\":false}]}")))
    (funcall success :data rsp))
  nil)

(cl-defun mock-request-call-2 (_url &rest settings
                                    &key
                                    (_params nil)
                                    (_data nil)
                                    (_headers nil)
                                    (_encoding 'utf-8)
                                    (_error nil)
                                    (_sync nil)
                                    (_response (make-request-response))
                                    &allow-other-keys)
  (let ((success (plist-get settings :success))
        (rsp (json-parse-string "{\"type\":\"feed\",\"name\":\"Webmentions\",\"children\":[]}")))
    (funcall success :data rsp))
  nil)

(cl-defun mock-request-call-3 (_url &rest settings
                                    &key
                                    (_params nil)
                                    (_data nil)
                                    (_headers nil)
                                    (_encoding 'utf-8)
                                    (_error nil)
                                    (_sync nil)
                                    (_response (make-request-response))
                                    &allow-other-keys)
  (let ((success (plist-get settings :success))
        (rsp (json-parse-string "{\"type\":\"feed\",\"name\":\"Webmentions\",\"children\":[{\"type\":\"entry\",\"author\":{\"type\":\"card\",\"name\":\"\",\"photo\":\"\",\"url\":\"\"},\"url\":\"https://www.reddit.com/r/planetemacs/comments/lt4c5l/elfeedscore_gnusstyle_scoring_for_elfeed/\",\"published\":null,\"wm-received\":\"2022-08-29T17:40:21Z\",\"wm-id\":1499344,\"wm-source\":\"https://www.reddit.com/r/planetemacs/comments/lt4c5l/elfeedscore_gnusstyle_scoring_for_elfeed/\",\"wm-target\":\"https://www.unwoundstack.com/doc/elfeed-score/curr\",\"mention-of\":\"https://www.unwoundstack.com/doc/elfeed-score/curr\",\"wm-property\":\"mention-of\",\"wm-private\":false,\"rels\":{\"canonical\":\"https://www.reddit.com/r/planetemacs/comments/lt4c5l/elfeedscore_gnusstyle_scoring_for_elfeed/\"}},{\"type\":\"entry\",\"author\":{\"type\":\"card\",\"name\":\"my test site\",\"photo\":\"\",\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/\"},\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-3.html\",\"published\":\"2022-06-05T10:49:00\",\"wm-received\":\"2022-06-05T22:35:37Z\",\"wm-id\":1410286,\"wm-source\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-3.html\",\"wm-target\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"name\":\"Indiweb Markup\",\"content\":{\"html\":\"<p><a href=\\\"https://www.unwoundstack.com\\\">unwoundstack</a> is now <a href=\\\"https://www.unwoundstack.com/blog/indieweb-markup.html\\\" class=\\\"u-in-reply-to\\\">using</a> microformats-- huzzah!</p>\",\"text\":\"unwoundstack is now using microformats-- huzzah!\"},\"in-reply-to\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"wm-property\":\"in-reply-to\",\"wm-private\":false},{\"type\":\"entry\",\"author\":{\"type\":\"card\",\"name\":\"my test site\",\"photo\":\"\",\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/\"},\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-2.html\",\"published\":\"2022-06-05T10:49:00\",\"wm-received\":\"2022-06-05T17:52:23Z\",\"wm-id\":1410220,\"wm-source\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-2.html\",\"wm-target\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"name\":\"Indiweb Markup\",\"content\":{\"html\":\"<p><a href=\\\"https://www.unwoundstack.com\\\">unwoundstack</a> is now <a href=\\\"https://www.unwoundstack.com/blog/indieweb-markup.html\\\">using</a> microformats-- huzzah!</p>\",\"text\":\"unwoundstack is now using microformats-- huzzah!\"},\"mention-of\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"wm-property\":\"mention-of\",\"wm-private\":false},{\"type\":\"entry\",\"author\":{\"type\":\"card\",\"name\":\"\",\"photo\":\"\",\"url\":\"\"},\"url\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-1.html\",\"published\":null,\"wm-received\":\"2022-06-05T14:39:13Z\",\"wm-id\":1410172,\"wm-source\":\"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-1.html\",\"wm-target\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"name\":\"Indiweb Markup\",\"content\":{\"html\":\"<p><a href=\\\"https://www.unwoundstack.com\\\">unwoundstack</a> is now <a href=\\\"https://www.unwoundstack.com/blog/indieweb-markup.html\\\">using</a> microformats-- huzzah!</p>\",\"text\":\"unwoundstack is now using microformats-- huzzah!\"},\"mention-of\":\"https://www.unwoundstack.com/blog/indieweb-markup.html\",\"wm-property\":\"mention-of\",\"wm-private\":false}]}")))
    (funcall success :data rsp))
  nil)

(ert-deftest indie-org-tests-check-webmentions ()
  "Test `indie-org-check-webmentions'."

  (let ((state
         (plist-get
          (indie-org-make-publication-state)
          :webmentions-received)))
    (advice-add 'request :before-while #'mock-request-call-1)
    (setq state (indie-org-check-webmentions "www.unwoundstack.com" "token" state))
    ;; Should probably use `unwind-protect' here, but I'm afraid it will
    ;; interfere with ert.
    (advice-remove 'request #'mock-request-call-1)
    (should (plist-get state :last-checked))
    (should (eq 1410286 (plist-get state :last-id)))

    (advice-add 'request :before-while #'mock-request-call-2)
    (setq state (indie-org-check-webmentions "www.unwoundstack.com" "token" state))
    ;; Should probably use `unwind-protect' here, but I'm afraid it will
    ;; interfere with ert.
    (advice-remove 'request #'mock-request-call-2)
    (should (plist-get state :last-checked))
    (should (eq 1410286 (plist-get state :last-id)))
    (let ((wms (gethash "blog/indieweb-markup.html" (plist-get state :mentions))))
      (should (eq (length wms) 3)))))

(ert-deftest indie-org-tests-check-issue-1 ()
  "Test issue #1 regressions."
  (let  ((state
         (plist-get
          (indie-org-make-publication-state)
          :webmentions-received)))
    (advice-add 'request :before-while #'mock-request-call-3)
    (setq state (indie-org-check-webmentions "www.unwoundstack.com" "token" state))
    ;; Should probably use `unwind-protect' here, but I'm afraid it will
    ;; interfere with ert.
    (advice-remove 'request #'mock-request-call-3)
    (should (plist-get state :last-checked))
    (should (eq 1499344 (plist-get state :last-id)))))

(cl-defun mock-request-issue-2 (_url &rest settings
                                    &key
                                    (_params nil)
                                    (_data nil)
                                    (_headers nil)
                                    (_encoding 'utf-8)
                                    (_error nil)
                                    (_sync nil)
                                    (_response (make-request-response))
                                    &allow-other-keys)
  (let ((err (plist-get settings :error))
        (rsp (json-parse-string "{\"error\":\"not_supported\",\"error_description\":\"The target domain is known to not accept webmentions.\"}")))
    (funcall err :data rsp :error-thrown '(error . "http 400")))
  nil)

(ert-deftest indie-org-test-check-issue-2 ()
  "Test regressions for issue #2."
  (let ((state
         (plist-get
          (indie-org-make-publication-state)
          :webmentions-received)))
    (advice-add 'request :before-while #'mock-request-issue-2)
    (let ((rsp (should-error
                (indie-org-send-webmention '("www.unwoundstack.com" . "https://github.com") "token")
                :type 'error))) 
      ;; Should probably use `unwind-protect' here, but I'm afraid it will
      ;; interfere with ert.
      (advice-remove 'request #'mock-request-issue-2))))

(provide 'indie-org-tests)

;;; indie-org-tests.el ends here
