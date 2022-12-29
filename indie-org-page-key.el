;;; indie-org-page-key.el -- Support for "page keys" in indie-org  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Michael Herstine <sp1ff@pobox.com>

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

;; This package defines the indie-org primitive "page-key".  Throughout
;; indie-org, we need a way to "name" pages on the site.  For the most part,
;; indie-org requires a page-key to simply be a string that uniquely identifies
;; each page on the site.  There are a few instances, however, in which we
;; need a bit more structure:

;; 1. It must be possible to map an URL naming the page back to a page-key (when
;; retrieving Webmentions)

;; 2. At page publication time, we need to know the page key corresponding to
;; the page being published is/will be (for recording Webmentions to be sent)

;; It would be possible to make this parameterizable by, say, providing
;; customizable variables holding functions for performing these two
;; operations.  As a first implementation, however, I will simply define "page
;; key" in the obivous way: as the page's path in a URL naming it on the site
;; (i.e. if the page lives at "https://foo.net/a/b/c.html" on your site, its
;; page-key shall be "a/b/c.html").

;;; Code:

(defun indie-org-page-key-at-pub (info)
  "Retrieve the page key from the communications channel INFO.
The \"page key\" is the path of the output file, relative to the
publication root (so, \"blog/cool-post.html\", for instance).

The channel must have been provisioned with the
:indie-org/publishing-root property."
  (let ((output-file (plist-get info :output-file))
        (project-directory
         (file-name-as-directory
          (plist-get info :indie-org/publishing-root))))
    (and (string-prefix-p project-directory output-file)
         (substring output-file (length project-directory)))))

(defun indie-org-page-key-from-url (domain-with-authority target)
  "Derive a page-key from a Webmention target.
DOMAIN-WITH-AUTHORITY is the \"scheme\" along with the top-level
domain (https://foo.net, e.g).  TARGET is the target of a
Webmetion (https://foo.net/a/b/c.html, e.g.)."
  (and (string-prefix-p domain-with-authority target)
       (substring target (1+ (length domain-with-authority)))))


(provide 'indie-org-page-key)
;;; indie-org-page-key.el ends here.
