;;; indie-org-posse-tests.el --- ERT tests for indie-org-posse   -*- lexical-binding: t; -*-

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

;; Unit tests for indie-org-posse.el.

;;; Code:

(require 'indie-org-posse)

(require 'request)

(ert-deftest indie-org-posse-tests-smoke ()
  "Smoke tests for indie-org-posse."
  (should (eq (indie-org-posse-string-to-target "twitter") :twitter))
  (should (equal (indie-org-posse-target-to-string :twitter) "twitter"))
  (should (equal (indie-org-posse-target-to-wm-target :twitter) "https://brid.gy/publish/twitter")))

(ert-deftest indie-org-posse-tests-posse-sent ()
  "Smoke test for POSSE requests sent."
  (let* ((requests (indie-org-posse-make-requests))
         (responses (indie-org-posse-make-responses)))
    ;; Record a few POSSE requests
    (indie-org-record-posse-request "twitter mastodon" "index.html" requests)
    (indie-org-record-posse-request "reddit" "mp3.html" requests)

    (let ((to-send (indie-org-posse-required requests responses)))
      (should (eq 2 (length to-send))) ;; two pages :=> requests
      ;; Fake sending one & check that it gets recorded properly
      (indie-org-record-sent-posse
       "index.html"
       (indie-org-posse-make-response
        :sort :twitter
        :created-at"Tue Jul 05 00:15:32 +0000 2022"
        :id "1544112708181794821"
        :text"unwoundstack can now send &amp; receive Webmentions: https://t.co/23cPkyQoZ0"
        :url "https://twitter.com/unwoundstack/status/1544112708181794821")
       responses)
      (let ((to-send
             (indie-org-posse-required requests responses)))
        (should (eq 2 (length to-send))) ;; two pages :=> requests
        ;; Fake recording the other one for index.html
        (indie-org-record-sent-posse
         "index.html"
         (indie-org-posse-make-response
          :sort :mastodon
          :created-at"Tue Jul 05 00:16:32 +0000 2022"
          :id "1544112708181794822"
          :text"unwoundstack can now send &amp; receive Webmentions: https://t.co/23cPkyQoZ0"
          :url "https://indieweb.social/@sp1ff/1544112708181794821")
         responses)
        (let ((to-send
               (indie-org-posse-required requests responses)))
          (should (eq 1 (length to-send))) ;; one remaining page :=> requests
          ;; record the last one
          (indie-org-record-sent-posse
           "mp3.html"
           (indie-org-posse-make-response
            :sort :reddit
            :created-at"Tue Jul 05 00:17:32 +0000 2022"
            :id "1544112708181794823"
            :text"unwoundstack can now send &amp; receive Webmentions: https://t.co/23cPkyQoZ0"
            :url "https://reddit.com/r/rust/not_a_real_title.html")
           responses)
          (let ((to-send
                 (indie-org-posse-required requests responses)))
            (should (eq 0 (length to-send)))
            ))))))

(cl-defun mock-send-posse-request-1 (_url &rest settings
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
  (let ((complete (plist-get settings :complete))
        (rsp (json-parse-string "{\"created_at\": \"Tue Jul 05 00:15:32 +0000 2022\",\"id\": \"1544112708181794821\",\"id_str\": \"1544112708181794821\",\"text\": \"unwoundstack can now send &amp; receive Webmentions: https://t.co/23cPkyQoZ0\",\"truncated\": false,\"entities\": {\"hashtags\": [],\"symbols\": [],\"user_mentions\": [],\"urls\": [{\"url\": \"https://t.co/23cPkyQoZ0\",\"expanded_url\": \"https://www.unwoundstack.com/blog/webmentions-ann.html\",\"display_url\": \"unwoundstack.com/blog/webmentio\u2026\",\"indices\": [53,76]}]},\"source\": \"<a href=\\\"https://brid.gy/\\\" rel=\\\"nofollow\\\">Bridgy</a>\",\"in_reply_to_status_id\": null,\"in_reply_to_status_id_str\": null,\"in_reply_to_user_id\": null,\"in_reply_to_user_id_str\": null,\"in_reply_to_screen_name\": null,\"user\": {\"id\": 49030875,\"id_str\": \"49030875\",\"name\": \"Michael Herstine\",\"screen_name\": \"unwoundstack\",\"location\": \"San Francisco Bay Area\",\"description\": \"I hack in C++, LISP & Rust. I think a lot about writing provably correct code.\",\"url\": \"https://t.co/BKytB746LW\",\"entities\": {\"url\": {\"urls\": [{\"url\": \"https://t.co/BKytB746LW\",\"expanded_url\": \"https://www.unwoundstack.com/\",\"display_url\": \"unwoundstack.com\",\"indices\": [0,23]}]},\"description\": {\"urls\": []}},\"protected\": false,\"followers_count\": 43,\"friends_count\": 553,\"listed_count\": 1,\"created_at\": \"Sat Jun 20 15:23:59 +0000 2009\",\"favourites_count\": 562,\"utc_offset\": null,\"time_zone\": null,\"geo_enabled\": false,\"verified\": false,\"statuses_count\": 435,\"lang\": null,\"contributors_enabled\": false,\"is_translator\": false,\"is_translation_enabled\": false,\"profile_background_color\": \"C0DEED\",\"profile_background_image_url\": \"http://abs.twimg.com/images/themes/theme1/bg.png\",\"profile_background_image_url_https\": \"https://abs.twimg.com/images/themes/theme1/bg.png\",\"profile_background_tile\": false,\"profile_image_url\": \"http://pbs.twimg.com/profile_images/273047982/Calvin-gots-an-Idea_109x119_normal.jpg\",\"profile_image_url_https\": \"https://pbs.twimg.com/profile_images/273047982/Calvin-gots-an-Idea_109x119_normal.jpg\",\"profile_banner_url\": \"https://pbs.twimg.com/profile_banners/49030875/1482024216\",\"profile_link_color\": \"1DA1F2\",\"profile_sidebar_border_color\": \"C0DEED\",\"profile_sidebar_fill_color\": \"DDEEF6\",\"profile_text_color\": \"333333\",\"profile_use_background_image\": true,\"has_extended_profile\": false,\"default_profile\": true,\"default_profile_image\": false,\"following\": false,\"follow_request_sent\": false,\"notifications\": false,\"translator_type\": \"none\",\"withheld_in_countries\": []},\"geo\": null,\"coordinates\": null,\"place\": null,\"contributors\": null,\"is_quote_status\": false,\"retweet_count\": 0,\"favorite_count\": 0,\"favorited\": false,\"retweeted\": false,\"possibly_sensitive\": false,\"lang\": \"en\",\"type\": \"post\",\"url\": \"https://twitter.com/unwoundstack/status/1544112708181794821\"}" :object-type 'alist)))
    (funcall complete :data rsp :symbol-status 'success))
  nil)

(ert-deftest indie-org-posse-tests-send-smoke ()
  "Smoke tests for `indie-org-posse-send'."
  (advice-add 'request :before-while #'mock-send-posse-request-1)
  (let ((rsp (indie-org-send-posse-request "https://my-post.foo.com" :twitter)))
    (should (eq :twitter (indie-org-posse-response-sort rsp)))
    (should (equal "Tue Jul 05 00:15:32 +0000 2022" (indie-org-posse-response-created-at rsp)))
    (should (equal "1544112708181794821" (indie-org-posse-response-id rsp)))))


(provide 'indie-org-posse-tests)
;;; indie-org-posse-tests.el ends here
