;;; indie-org-pub.el --- indie-org publication support  -*- lexical-binding: t; -*-

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

;;; Code
(require 'indie-org-page-key)
(require 'indie-org-posse)
(require 'indie-org-updated)
(require 'indie-org-webmentions)

(require 'ox-publish)

;;; Code:

(defun indie-org-pub-finalize-page (backend info)
  "Collect all webmentions & update state.
BACKEND is the Org export backend.
INFO is a plist used as a communications channel.

This function must be called after each page is exported.  It
will grab all webmentions from the :indie-org/mentions INFO
property and update the publication state.  `indie-org-enable'
will add it to `org-export-filter-final-output-functions'."
  (when (org-export-derived-backend-p backend 'indie-org)
    (let* ((mentions (plist-get info :indie-org/mentions))
           (page-key (indie-org-page-key-at-pub info))
           ;; 👇 needs to be an indie-org-webmentions-made instance
           (webmentions-made (plist-get info :indie-org/webmentions-made))
           (input-file (plist-get info :input-file))
           ;; `info' is different than `project'! It has no car consisting of
           ;; the project name, which `org-publish-find-date' requires. That
           ;; said, it's already been parsed into the plist, so we can retrieve
           ;; it there.
           (publish-date
            (let* ((data (plist-get info :date))
                   (raw-data (if data (org-element-property :raw-value (car data)))))
              (when raw-data (org-time-string-to-time raw-data))))
           (updated
            (let* ((data (plist-get info :updated))
                   (raw-data (if data (org-element-property :raw-value (car data)))))
              (when raw-data (org-time-string-to-time raw-data)))))
      ;; `mentions' is just a list of URLs I've mentioned on this page.
      (indie-org-webmentions-update-made
       webmentions-made
       page-key
       (or updated publish-date)
       mentions))))

(defun indie-org-pub-find-posts (project-dir exclude-drafts &rest kwargs)
  "Determine which posts in PROJECT-DIR are to be included.
Set EXCLUDE-DRAFTS to t to exclude drafts.
KWARGS recognizes :exclude; a list of files to be excluded, along
with :tags-- a list of strings.  If non-nil, only posts with one
or more of these tags will be included in the feed.

`indie-org' introduces the export option DRAFT which can be used
to exclude posts from publication.  Use this method to assemble a
list of posts suitable for use with the :include Org Export
project property.

PROJECT-DIR shall be a directory containing posts.  If
EXCLUDE-DRAFTS is t, any post with a #+DRAFT keyword set to t
will be excluded (else all posts will be included).  The optional
keyword argument :exclude can be used to provide a list of files
to be excluded-- this is handy for sitemaps, e.g."
  (let ((excludes (plist-get kwargs :exclude))
        (tags (plist-get kwargs :tags))
        ;; This is a bit of a hack: `org-publish-find-property'
        ;; requires the Org Export project, which is really
        ;; irritating, since this function is presumably being used to
        ;; build-up the project in the first place.  From inspection,
        ;; the only project property used is the :base-directory,
        ;; which we know already, so we just fake it out.
        (pseudo-project (list "pseudo" :base-directory project-dir))
        (out))
    (mapc
     (lambda (dirent)
       (if (and (not (member dirent excludes))
                (or (not exclude-drafts)
                    (not (org-publish-find-property
                          dirent :draft pseudo-project 'indie-org)))
                (or (not tags)
                    (seq-intersection
                     tags
                     (org-publish-find-property dirent :filetags pseudo-project 'indie-org))))
           (setq out (cons dirent out))))
     (directory-files project-dir nil ".*\\.org$"))
    out))

(defun indie-org-pub-pp-drafts (posts-dir &rest kwargs)
    "Pretty-print drafts in POSTS-DIR.
KWARGS recognizes :exclude; a list of files to be excluded."
  (let ((published)
        (drafts)
        (excludes (plist-get kwargs :exclude))
        (pseudo-project (list "pseudo" :base-directory posts-dir)))
    (org-publish-initialize-cache "pseudo")
    (mapc
     (lambda (dirent)
       (unless (member dirent excludes)
         (let ((tm (float-time (indie-org-updated-time-or-date dirent pseudo-project))))
           (if (org-publish-find-property dirent :draft pseudo-project 'indie-org)
               (setq drafts (cons (cons tm dirent) drafts))
             (setq published (cons (cons tm dirent) published))))))
     (directory-files posts-dir nil ".*\\.org$"))
    (message "drafts: %s" drafts)
    (let ((published (mapcar #'cdr (sort published :key #'car :lessp #'>)))
          (drafts (mapcar #'cdr (sort drafts :key #'car :lessp #'>))))
      (message "Published:")
      (while published
        (message "    %s" (car published))
        (setq published (cdr published)))
      (message "Drafts:")
      (while drafts
        (message "    %s" (car drafts))
        (setq drafts (cdr drafts))))))

(defun indie-org-pub-webmentions-received-at-pub (info received)
  "Retrieve the webmentions received for a page at publication time.
INFO shall be the communications channel used by Org export.
RECEIVED shall be an `indie-org-webmentions-received' instance."
  (if received
      (progn
        (unless (indie-org-webmentions-received-p received)
          (signal 'wrong-type-argument (list #'indie-org-webmentions-received-p received)))
        (gethash
         (indie-org-page-key-at-pub info)
         (indie-org-webmentions-received-mentions received)))))

(defun indie-org-pub-posse-responses-at-pub (info responses)
  "Retrieve POSSE responses for a page at publication time.
INFO shall be the communications channel used by Org export.
RESPONSES shall be an `indie-org-posse-responses' instance."
  (if responses
      (progn
        (unless (indie-org-posse-responses-p responses)
          (signal 'wrong-type-argument (list #'indie-org-posse-responses-p responses)))
        (gethash
         (indie-org-page-key-at-pub info)
         (indie-org-posse-responses-hash responses)))))

;;;###autoload
(defun indie-org-pub-enable ()
  "Enable indie-org publication."
  (org-export-define-derived-backend 'indie-org 'html
    ;; Cf. `org-export-options-alist'.  Each entry is an association
    ;; between a property name & how it's set.  The latter is a list:
    ;; (KEYWORD OPTION DEFAULT BEHAVIOR)
    ;; BEHAVIOR describes how Org should behave in the presence of
    ;; multiple instances of the keyword, and shall be one of:
    ;; nil       Keep old value and discard the new one.
    ;; t         Replace old value with the new one.
    ;; ‘space’   Concatenate the values, separating them with a space.
    ;; ‘newline’ Concatenate the values, separating them with
	  ;;     a newline.
    ;; ‘split’   Split values at white spaces, and cons them to the
	  ;;     previous list.
    ;; ‘parse’   Parse value as a list of strings and Org objects,
    ;;           which can then be transcoded with, e.g.,
    ;;           ‘org-export-data’.  It implies ‘space’ behavior.
    :options-alist '((:draft "DRAFT" nil nil t)
                     (:updated "UPDATED" nil nil parse)
                     (:posse "POSSE" nil nil split)))
  (add-to-list
   'org-export-filter-final-output-functions
   ;; BUG-- I think this function "must return a string that will be used as
   ;; the final export output"
   (lambda (_text backend info)
     (indie-org-pub-finalize-page backend info))))

(provide 'indie-org-pub)
;;; indie-org-pub.el ends here.
