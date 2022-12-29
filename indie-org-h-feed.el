;;; indie-org-h-feed.el --- indie-org h-feed support  -*- lexical-binding: t; -*-

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

;; None.

;;; Code:
(require 'ox)

(defun indie-org-h-feed--publish (plist filename pub-dir)
  "Publish an h-feed with PLIST, only when FILENAME is the sitemap file.
PUB-DIR is when the output will be placed."
  (let ((sitemap-filename (plist-get plist :sitemap-filename)))
    (if (equal sitemap-filename (file-name-nondirectory filename))
        (let ((bf (get-file-buffer filename)))
          (if bf
	            (with-current-buffer bf
	              (write-file filename))
            (find-file filename)
            (write-file filename)
            (kill-buffer))
          (org-publish-org-to
           (plist-get plist :h-feed-backend)
           filename ".html"
           plist pub-dir)))))

(defun indie-org-h-feed--format-entry (entry style project)
  "Format ENTRY for an h-feed.
ENTRY is a file name.  STYLE is either `list' or `tree'.  PROJECT
is the current project.

This function is suitable for use as a :sitemap-format-entry
value.  I use it to transcode a sitemap entry from a bulleted
list item to a level one headline.  This let's me attach assorted
properties to each item that I can use when I publish to HTML."
  (let ((dsc (org-publish-find-property entry :description project 'indie-org-h-feed)))
    (if (or (not dsc) (and (stringp dsc) (eq 0 (length dsc))))
        (error "You forgot the description for %s" entry)
      (cond
       ((not (directory-name-p entry))
	      (let ((link (concat (file-name-sans-extension entry) ".html"))
              (title (org-publish-find-title entry project))
              (date (format-time-string
                     "%Y-%m-%d %H:%M"
                     (org-publish-find-date entry project)))
              (updated
               (let ((val
                      (org-publish-find-property entry :updated project 'indie-org)))
                 (if val
                     (format-time-string
                      "%Y-%m-%d %H:%M"
                      (org-time-string-to-time (org-element-interpret-data val)))))))
          (with-temp-buffer
            (org-mode)
            (insert (format "* [[file:%s][%s]]\n" entry title))
            (org-insert-property-drawer)
            (org-set-property "U_URL" link)
            (org-set-property "P_NAME" title)
            (org-set-property "PUBDATE" date)
            (org-set-property "P_SUMMARY" dsc)
            (if updated
                (org-set-property
                 "UPDATED"
                 updated))
            (insert
             (format
              "%s\n"
              (org-no-properties (org-element-interpret-data dsc))))
            (buffer-string))))
	     ((eq style 'tree)
	      ;; Return only last subdir.
	      (file-name-nondirectory (directory-file-name entry)))
	     (t entry)))))

(defun indie-org-h-feed--format-sitemap (title list)
  "Generate h-feed, as a string.
TITLE is the title of the feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.

It can further be transformed using `org-list-to-generic',
`org-list-to-subtree' and alike.  See also
`org-publish-sitemap-default'.

Suitable for use as an argument to :sitemap-function"
  (concat "#+TITLE: " title "\n"
          (org-list-to-subtree list 0 (list :icount "" :istart ""))))

(defun indie-org-h-feed-enable-full-h-feed (project sitemap-filename feed-title feed-description)
  "Add a full h-feed to an Org Export PROJECT.
SITEMAP-FILENAME will be used as the Org export property :sitemap-filename.
FEED-TITLE will be used as the h-feed title.
FEED-DESCRIPTION will be used as the h-feed description.

This function will return a list suitable for use as an Org
Export project that will produce an h-feed.  The caller is
expected to initialize PROJECT as they see fit (setting the
project name and properties such as :base-directory,
:publishing-directory, and so forth.  This method will (ab)use
the sitemap feature to produce an HTML fragment containing an
h-feed for the posts in :base-directory.

BACKEND is a caller-supplied backend, presumably with a
caller-generated :template property.

The caller will presumably include this in their HTML template
elsewhere."
  (let ((name (car project))
        (plist (copy-sequence (cdr project))))
    (setq plist (plist-put plist :auto-sitemap t))
    (setq plist (plist-put plist :sitemap-filename sitemap-filename))
    (setq plist (plist-put plist :publishing-function #'indie-org-h-feed--publish))
    (setq plist (plist-put plist :sitemap-format-entry #'indie-org-h-feed--format-entry))
    (setq plist (plist-put plist :sitemap-sort-files 'anti-chronologically))
    (setq plist (plist-put plist :sitemap-function #'indie-org-h-feed--format-sitemap))
    (setq plist (plist-put plist :hfeed-name feed-title))
    (setq plist (plist-put plist :description feed-description))
    (setq plist (plist-put plist :h-feed-backend 'indie-org-h-feed))
    (setq plist (plist-put plist :with-toc nil))
    (cons name plist)))

;;;###autoload
(defun indie-org-h-feed-enable ()
  "Enable indie-org h-feed support."
  (org-export-define-derived-backend 'indie-org-h-feed 'html
    ;; h-feed core properties
    :options-alist
    '((:hfeed-name "HFEED_NAME" nil nil newline)
      (:hfeed-url "HFEED_URL" nil nil t)
      (:hfeed-photo "HFEED_PHOTO" nil nil t))))

(provide 'indie-org-h-feed)
;;; indie-org-h-feed.el ends here.
