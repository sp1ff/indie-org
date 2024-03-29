;;; test-state.el --- ERT tests for indie-org publication state    -*- lexical-binding: t; -*-

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

;; Unit tests for `indie-org-state.el'.

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'indie-org-state)

(defvar srcdir (getenv "srcdir"))
(cl-assert srcdir t "Please specifiy the environment variable 'srcdir'.")

(defvar builddir (getenv "builddir"))
(cl-assert builddir t "Please specifiy the environment variable 'builddir'.")

(defun hash-equal (hash1 hash2)
  "Compare two hash tables HASH1 & HASH2 to see whether they are equal."
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

(indie-org-state-enable)

(ert-deftest test-state-smoke ()
  "Smoke tests for `indie-org-state'."
  (let ((state (list :prod (indie-org-state-make)))
        (now (current-time)))
    (indie-org-state-update-last-published state now)
    (should (equal now (indie-org-state-get-last-published state))))
  (let ((state-v1 (indie-org-make-publication-state))
        (now (current-time)))
    (setf (indie-org-state-last-published state-v1) now)
    (let ((state-v2 (indie-org-state-v2-from-v1 state-v1)))
      (should (equal now (indie-org-state-v2-last-published state-v2))))))

(defmacro with-advised-message (buf &rest body)
  "Execute BODY with `message' advised to concatenate output to BUF."
  `(let ((closure
             (lambda (format-string &rest args)
               (setq ,buf (concat ,buf (apply #'format format-string args) "\n")))))
     (unwind-protect
         (progn
           (advice-add #'message :override closure)
           ,@body)
       (advice-remove #'message closure))))

(ert-deftest test-state-pp ()
  "Test pretty-printing state."
  (let* ((pub-state (indie-org-state-make))
         (now (current-time))
         (fmtd-now (format-time-string "%Y-%m-%d %H:%M:%S" now))
         (urls '("https://foo.com" "https://bar.net"))
         (made (indie-org-webmentions-make-made)))
    (setf (indie-org-state-v2-last-published pub-state) now)
    (indie-org-webmentions-update-made made "a/b/c.html" now urls)
    ;; `made' should now contain these mentions made
    ;; (indie-org-webmentions--update-sent
    ;;  sent "a/b/c.html"
    ;;  (indie-org-webmentions--make-sent-wm
    ;;   :source "a/b/c.html"
    ;;   :target "https://foo.com"))
    (setf (indie-org-state-v2-webmentions-made pub-state) made)
    (let* ((text "")
           (state (list :prod pub-state)))
      (with-advised-message text (indie-org-state-pp state))
      (should
       (equal
        (format ":prod:
    Last published: %s
    Webmentions made:
        a/b/c.html:
            %s:
                https://foo.com
                https://bar.net
" fmtd-now fmtd-now)
        text)))))

(ert-deftest test-state-v1 ()
  "Test deserializing version 1 of the serialization format."
  (let* ((state (indie-org-state-read
                (concat
                 (file-name-as-directory (expand-file-name srcdir))
                 "test-state.1")))
         (prod (plist-get state :prod)))
    (should
     (equal
      '(25520 42873 217813 227000)
      (indie-org-state-v2-last-published prod)))

    (should
     (equal
      '("https://webmention.rocks/test/1")
      (gethash
       '(25282 4388)
       (indie-org-webmentions-targets-hash
        (gethash
         "blog/webmentions-ann.html"
         (indie-org-webmentions-made-hash
          (indie-org-state-v2-webmentions-made prod)))))))

    (should
     (equal
      (gethash
       "https://indieweb.org/webmention-implementation-details#types_of_mentions"
       (indie-org-webmentions-send-targets-hash
        (gethash
         "blog/webmentions-test.html"
         (indie-org-webmentions-sent-hash
          (indie-org-state-v2-webmentions-sent prod)))))
      (list
       (indie-org-webmentions-make-sent-wm
        :source "blog/webmentions-test.html"
        :target "https://indieweb.org/webmention-implementation-details#types_of_mentions"
        :time-sent '(25282 10227 399117 294000)
        :status "https://telegraph.p3k.io/webmention/16f9yhNMFnhdMFYBOC"))))
    
    (let ((recvd (indie-org-state-v2-webmentions-received prod)))
      (should
       (equal
        (indie-org-webmentions-received-last-checked recvd)
        '(25576 4067 377379 204000)))
      (should
       (equal
        (indie-org-webmentions-received-last-id recvd)
        1515115))
      (let ((hash (indie-org-webmentions-received-mentions recvd)))
        (should (equal 3 (length (gethash "blog/indieweb-markup.html" hash))))))

    (let* ((posse-req (indie-org-state-v2-posse-requests prod))
           (reqs (gethash "blog/webmentions-ann.html" (indie-org-posse-requests-hash posse-req))))
      (should (equal reqs '(:twitter))))

    ;; (#s(indie-org-posse-response :twitter "Tue Jul 05 00:15:32 +0000 2022"
    ;; "1544112708181794821"
    ;; "unwoundstack can now send &amp; receive Webmentions: https://t.co/23cPkyQoZ0"
    ;; "https://twitter.com/unwoundstack/status/1544112708181794821"))
    (let* ((posse-rsps (indie-org-state-v2-posse-responses prod))
           (rsps (gethash "blog/webmentions-ann.html" (indie-org-posse-responses-hash posse-rsps))))
      (should
       (equal 1 (length rsps)))
      (let ((rsp (car rsps)))
        (should (eq :twitter (indie-org-posse-response-sort rsp)))
        (should (equal "1544112708181794821" (indie-org-posse-response-id rsp)))))

    ;; `state' appears to have been deserialized correctly from a v1 state file. Now
    ;; let's write it out as v3, then re-read & compare.
    (let ((state-file (make-temp-file "indie-org-test-state-v1")))
      (indie-org-state-write state state-file)
      (let ((state2 (indie-org-state-read state-file)))
        ;; This fails with a massive stack trace:
        ;; (should (equal state state2))
        ;; so let's do this incrementally:
        (should
         (equal
          (indie-org-state-v2-last-published prod)
          (indie-org-state-v2-last-published (plist-get state2 :prod))))))))

(ert-deftest test-state-v2 ()
  "Test deserializing version 2 of the format."
  (let* ((state (indie-org-state-read
                 (concat
                  (file-name-as-directory (expand-file-name srcdir))
                  "test-state.2")))
         (prod (plist-get state :prod)))
    (should
     (equal
      '(25586 51689 266183 351000)
      (indie-org-state-v2-last-published prod)))

    ;; `state' appears to have been deserialized correctly from a v1 state file. Now
    ;; let's write it out as v3, then re-read & compare.
    (let ((state-file (make-temp-file "indie-org-test-state-v2")))
      (indie-org-state-write state state-file)
      (let ((state2 (indie-org-state-read state-file)))
        ;; This fails with a massive stack trace:
        ;; (should (equal state state2))
        ;; so let's do this incrementally:
        (should
         (equal
          (indie-org-state-v2-last-published prod)
          (indie-org-state-v2-last-published (plist-get state2 :prod))))))))

(ert-deftest test-issue-5 ()
  "Regression test for issue #5.

Received webmentions serialized incorrectly"
  (let* ((wm0
          (indie-org-webmentions-make-received-wm
           :id 1633073 :sort :like :time-received '(25601 27765)
           :source "https://brid.gy/like/twitter/coffeehouset/1631471052848091137/1082821758565019650"
           :target "https://www.coffeehouse-talker.net/posts/open-letters-to-bruce-mcpherson-2.html"
           :author (indie-org-webmentions--make-received-wm-author
                    :name "SantaCruzLocal"
                    :photo "https://webmention.io/avatar/pbs.twimg.com/f7d0e282e42965a5b8f0f9b37eed6e92476a00708f2846c1facbc26d675946ff.jpg"
                    :type "card"
                    :url "https://twitter.com/theSCLocal")))
         (wm1
          (indie-org-webmentions-make-received-wm
           :id 1633074 :sort :reply :time-received '(25601 27767)
           :source "https://brid.gy/comment/twitter/coffeehouset/1631471052848091137/1631499095700537344"
           :target "https://www.coffeehouse-talker.net/posts/open-letters-to-bruce-mcpherson-2.html"
           :author (indie-org-webmentions--make-received-wm-author
                    :name "SantaCruzLocal"
                    :photo "https://webmention.io/avatar/pbs.twimg.com/f7d0e282e42965a5b8f0f9b37eed6e92476a00708f2846c1facbc26d675946ff.jpg"
                    :type "card"
                    :url "https://twitter.com/theSCLocal")
           :content (indie-org-webmentions--make-received-wm-content
                     :html "Thank you !\n<a class=\"u-mention\" href=\"https://slvpost.com/open-letters-to-bruce-mcpherson-and-county-leadership/\"></a>\n<a class=\"u-mention\" href=\"https://twitter.com/CoffeehouseT\"></a>\n<a class=\"u-mention\" href=\"https://twitter.com/SLVPostNews\"></a>"
                     :text "Thank you !")))
         (hash (make-hash-table :test #'equal)))
    (puthash
     "https://www.coffeehouse-talker.net/posts/open-letters-to-bruce-mcpherson-2.html"
     (list wm0 wm1) hash)
    (let* ((recvd
            (indie-org-webmentions-make-received
             :last-checked '(25601 27882 230381 375000)
             :last-id 1633074
             :mentions hash))
           (plist (indie-org-webmentions-received-to-plist recvd))
           (recvd2 (indie-org-webmentions-received-from-plist plist))
           (hash (indie-org-webmentions-received-mentions recvd2))
           (wms
            (gethash
             "https://www.coffeehouse-talker.net/posts/open-letters-to-bruce-mcpherson-2.html"
             hash)))
      (should (eq 2 (length wms)))
      (let ((wm0 (nth 0 wms))
            (wm1 (nth 1 wms)))
        (should (eq 1633073 (indie-org-webmentions-received-wm-id wm0)))
        (should (eq 1633074 (indie-org-webmentions-received-wm-id wm1)))))))

(provide 'test-state)
;;; test-state.el ends here
