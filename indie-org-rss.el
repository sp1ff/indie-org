;;; indie-org-rss.el --- indie-org RSS support  -*- lexical-binding: t; -*-

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

;; Support for adding an RSS feed to an indie-org-published site.

;; There is an RSS exporter that ships with Org-mode, albeit under the
;; "contrib" folder- `ox-rss'.  Unfortunately, it works by publishing *each
;; file* as RSS, with the convention that top-level entries map to RSS feed
;; items.  This is not what I want, since I write blog posts on a
;; one-entry-per-file basis.

;; I found an interesting workaround to this: setup an RSS project for the blog
;; that uses the sitemap feature to create a .org file describing all posts
;; that is suitable for export to RSS.  Then wrap the RSS export function to
;; skip RSS export for all files other than the sitemap file.

;;; Code:

(require 'ox)
(require 'ox-rss)

(defun indie-org-rss--publish (plist filename pub-dir)
  "Publish RSS 2.0 with PLIST, only when FILENAME is the sitemap file.
PUB-DIR is when the output will be placed."
  (let ((sitemap-filename (plist-get plist :sitemap-filename)))
    (if (equal sitemap-filename (file-name-nondirectory filename))
        (org-rss-publish-to-rss plist filename pub-dir))))

(defun indie-org-rss--format-entry (entry style project)
  "Format ENTRY for the RSS feed.

ENTRY is a file name.  STYLE is either `list' or `tree'.  PROJECT
is the current project."
  (let ((dsc (org-publish-find-property entry :description project 'indie-org)))
    (if (or (not dsc) (and (stringp dsc) (eq 0 (length dsc))))
        (error "You forgot the description for %s" entry)
      (cond
       ((not (directory-name-p entry))
	      (let ((link (concat (file-name-sans-extension entry) ".html"))
              (title (org-publish-find-title entry project))
              (date (format-time-string
                     "%Y-%m-%d"
                     (org-publish-find-date entry project))))
          (with-temp-buffer
            (org-mode)
            (insert (format "* [[file:%s][%s]]\n" entry title))
            (org-set-property "RSS_PERMALINK" link)
            (org-set-property "RSS_TITLE" title)
            (org-set-property "PUBDATE" date)
            (insert
             (format
              "%s\n"
              (org-no-properties (org-element-interpret-data dsc))))
            (buffer-string))))
	     ((eq style 'tree)
	      ;; Return only last subdir.
	      (file-name-nondirectory (directory-file-name entry)))
	     (t entry)))))

(defun indie-org-rss--format-sitemap (title list)
  "Generate the RSS sitemap file.
TITLE shall be the RSS 2.0 title.  LIST shall be a representation
of the files and directories involved in the project as a nested
list."
  (concat "#+TITLE: " title "\n"
   (org-list-to-subtree list 0 (list :icount "" :istart ""))))

(defun indie-org-rss-enable-rss-2.0 (project sitemap-filename feed-title feed-description)
  "Add an RSS 2.0 feed to an Org Export PROJECT.
SITEMAP-FILENAME will be used as the Org Export :sitemap-filename.
FEED-TITLE will be the RSS feed title.
FEED-DESCRIPTION will the RSS feed description.

This function will return a list suitable for use as an Org
Export project that will produce an RSS 2.0 feed.  The caller is
expected to initialize PROJECT as they see fit (setting the
project name and properties such as :base-directory,
:publishing-directory, and so forth.  This method will (ab)use
the sitemap feature to setup an RSS 2.0 feed for the posts in
:base-directory."
  (let ((name (car project))
        (plist (copy-sequence (cdr project))))
    (setq plist (plist-put plist :auto-sitemap t))
    (setq plist (plist-put plist :publishing-function #'indie-org-rss--publish))
    (setq plist (plist-put plist :sitemap-filename sitemap-filename))
    (setq plist (plist-put plist :sitemap-format-entry #'indie-org-rss--format-entry))
    (setq plist (plist-put plist :sitemap-sort-files 'anti-chronologically))
    (setq plist (plist-put plist :sitemap-function #'indie-org-rss--format-sitemap))
    (setq plist (plist-put plist :feed-title feed-title))
    (setq plist (plist-put plist :description feed-description))
    (cons name plist)))

(provide 'indie-org-rss)
;;; indie-org-rss.el ends here.

