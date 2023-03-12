;;; indie-org-webmentions-tests.el --- ERT tests for indie-org-webmentions   -*- lexical-binding: t; -*-

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

;; Unit tests for indie-org-webmentions.el.

;;; Code:

(require 'indie-org-webmentions)

(require 'request)

(require 'ox)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            general unit tests                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest indie-org-webmentions-tests-made-smoke ()
  "Webmention made smoke tests."

  (let* ((now (current-time))
         (urls '("https://foo.com" "https://bar.net"))
         (by-prod (indie-org-webmentions-make-made)))
    (indie-org-webmentions-update-made by-prod "page-key" now urls)
    ;; `made' should now contain these mentions made.
    (let ((hash (indie-org-webmentions-made-hash by-prod)))
      ;; Kind of a lame test, since I'm depending on implementation details of
      ;; the per-env webmentions-made struct, but it's all I've got rn.
      (should (eq (hash-table-count hash) 1)))))

(ert-deftest indie-org-webmentions-tests-sent-smoke ()
  "Webmentions sent smoke tests."

  (let* ((now (current-time))
         (by-prod (indie-org-webmentions-make-sent))
         (sent-wm (indie-org-webmentions-make-sent-wm
                   :source "foo.html" :target "https://bar.net/splat.html"
                   :time-sent now)))
    (indie-org-webmentions-update-sent by-prod "foo.html" "https://bar.net/splat.html" sent-wm)
    ;; `sent' should now contain this webmention sent
    (let ((hash (indie-org-webmentions-sent-hash by-prod)))
      ;; Kind of a lame test, since I'm depending on implementation details of
      ;; the per-env webmentions-sent struct, but it's all I've got rn.
      (should (eq (hash-table-count hash) 1)))))

(ert-deftest indie-org-webmentions-tests-mentions-sent ()
  "Smoke test for `indie-org-webmentions-required'."
  (let* ((now (current-time))
         (made-by-prod (indie-org-webmentions-make-made)))
    ;; Record two webmentions on index.html & no sent...
    (indie-org-webmentions-update-made made-by-prod "index.html" now '("https://foo.com" "https://bar.com"))
    (let* ((now (current-time))
           (sent-by-prod (indie-org-webmentions-make-sent))
           (to-send (indie-org-webmentions-required made-by-prod sent-by-prod)))
      ;; -- should require both to be sent.
      (should (eq 2 (length to-send)))
      (should
       (eq 0
           (length
            (cl-set-exclusive-or
             to-send
             '(("index.html" . "https://bar.com") ("index.html" . "https://foo.com"))))))
      ;; OK-- now let's simulate sending one Webmention...
      (indie-org-webmentions-record-sent '("index.html" . "https://bar.com") now sent-by-prod)
      (let ((to-send (indie-org-webmentions-required made-by-prod sent-by-prod)))
        (should (equal to-send '(("index.html" . "https://foo.com"))))
        ;; OK-- now let's send it again (imagine we updated the page or something)
        (let* ((now (current-time)))
          (indie-org-webmentions-record-sent '("index.html" . "https://bar.com")  now sent-by-prod)
          ;; Shouldn't change the WMs required to be sent:
          (let ((to-send (indie-org-webmentions-required made-by-prod sent-by-prod)))
            (should (equal to-send '(("index.html" . "https://foo.com")))))
          ;; OK-- now let's send the other one
          (let* ((now (current-time)))
            (indie-org-webmentions-record-sent '("index.html" . "https://foo.com") now sent-by-prod)
            (let ((to-send (indie-org-webmentions-required made-by-prod sent-by-prod)))
              (should (eq 0 (length to-send)))))))
      ;; OK, now let's imagine we updated the page to add a third webmention
      (let ((now (current-time)))
        (indie-org-webmentions-update-made made-by-prod "index.html"
                                           now '("https://foo.com" "https://splat.net" "https://bar.com"))
        (let ((to-send (indie-org-webmentions-required made-by-prod sent-by-prod)))
          ;; We've updated the page, so all webmentions now need to be re-sent
          (should
           (eq 0
               (length
                (cl-set-exclusive-or
                 to-send
                 '(("index.html" . "https://bar.com")
                   ("index.html" . "https://splat.net")
                   ("index.html" . "https://foo.com")))))))))))

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

(ert-deftest indie-org-webmentions-tests-check-issue-2 ()
  "Test regressions for issue #2."
  (advice-add 'request :before-while #'mock-request-issue-2)
  (should-error
   (indie-org-webmentions-send '("www.unwoundstack.com" . "https://github.com") "token")
   :type 'error)
  ;; Should probably use `unwind-protect' here, but I'm afraid it will
  ;; interfere with ert.
  (advice-remove 'request #'mock-request-issue-2))

;; Mock a `request' call to return 201 with the following body:

;; {
;;   "status": "queued",
;;   "summary": "Webmention was queued for processing",
;;   "location": "https://webmention.io/www.unwoundstack.com/webmention/IjvtV8Ce0wvSxB4mbVjL",
;;   "source": "http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-1.html",
;;   "target": "https://www.unwoundstack.com/blog/indieweb-markup.html"
;; }

(cl-defun mock-send-wm-request-call-1 (_url &rest settings
                                            &key
                                            (_params nil)
                                            (_data nil)
                                            (_headers nil)
                                            (_encoding 'utf-8)
                                            (_error nil)
                                            (_sync nil)
                                            (_response (make-request-response))
                                            &allow-other-keys)
  "Advise `request' to return a fixed response."
  (let ((success (plist-get settings :success))
        (rsp (json-parse-string "{\"status\": \"queued\",\"summary\": \"Webmention was queued for processing\",\"location\": \"https://webmention.io/www.unwoundstack.com/webmention/IjvtV8Ce0wvSxB4mbVjL\",\"source\": \"http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-1.html\",\"target\": \"https://www.unwoundstack.com/blog/indieweb-markup.html\" }" :object-type 'alist)))
    (funcall success :data rsp))
  nil)

(ert-deftest indie-org-webmentions-tests-send-smoke ()
  "Smoke tests for sending Webmentions."
  (advice-add 'request :before-while #'mock-send-wm-request-call-1)
  (should (equal
           "https://webmention.io/www.unwoundstack.com/webmention/IjvtV8Ce0wvSxB4mbVjL"
           (indie-org-webmentions-send '("source" . "target") "token")))
  (advice-remove 'request #'mock-send-wm-request-call-1))

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

(ert-deftest indie-org-webmentions-tests-check-webmentions ()
  "Test `indie-org-webmentions-check'."
  (let ((received (indie-org-webmentions-make-received)))
    (advice-add 'request :before-while #'mock-request-call-1)
    (indie-org-webmentions-check "www.unwoundstack.com" "token" received)
    ;; Should probably use `unwind-protect' here, but I'm afraid it will
    ;; interfere with ert.
    (advice-remove 'request #'mock-request-call-1)
    (should (indie-org-webmentions-received-last-checked received))
    (should (eq 1410286 (indie-org-webmentions-received-last-id received)))
    (let ((rcvd (indie-org-webmentions-received-for-page-key received "blog/indieweb-markup.html")))
      (should (eq 3 (length rcvd)))
      (should
       (equal
        "http://409569222777-useast2-test-webmentions.s3-website.us-east-2.amazonaws.com/sample-1.html"
        (indie-org-webmentions-received-wm-url (car rcvd)))))

    (advice-add 'request :before-while #'mock-request-call-2)
    (indie-org-webmentions-check "www.unwoundstack.com" "token" received)
    ;; Should probably use `unwind-protect' here, but I'm afraid it will
    ;; interfere with ert.
    (advice-remove 'request #'mock-request-call-2)
    (should (indie-org-webmentions-received-last-checked received))
    (should (eq 1410286 (indie-org-webmentions-received-last-id received)))
    (let ((wms (gethash "blog/indieweb-markup.html" (indie-org-webmentions-received-mentions received))))
      (should (eq (length wms) 3)))))

(cl-defun mock-request-issue-1 (_url &rest settings
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

(ert-deftest indie-org-webmentions-tests-check-issue-1 ()
  "Test issue #1 regressions."
  (let  ((received (indie-org-webmentions-make-received)))
    (advice-add 'request :before-while #'mock-request-issue-1)
    (indie-org-webmentions-check "www.unwoundstack.com" "token" received)
    ;; Should probably use `unwind-protect' here, but I'm afraid it will
    ;; interfere with ert.
    (advice-remove 'request #'mock-request-issue-1)
    (should (indie-org-webmentions-received-last-checked received))
    (should (eq 1499344 (indie-org-webmentions-received-last-id received)))))

(provide 'indie-org-webmentions-tests)
;;; indie-org-webmentions-tests.el ends here
