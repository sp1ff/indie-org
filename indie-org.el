;;; indie-org.el --- Org HTML Export on the Indieweb -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>
;; Version: 0.2.1
;; Package-Requires: ((emacs "24"))
;; Keywords: hypermedia, outlines, wp
;; URL: https://www.unwoundstack.com

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

;;; Code:
(require 'ox)
(require 'ox-rss)
(require 'request)

(defconst indie-org-version "0.2.1")

(defgroup indie-org nil
  "Org HTML Export on the Indieweb."
  :group 'org)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         RSS 2.0 Support                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There is an RSS exporter that ships with Org-mode, albeit under the
;; "contrib" folder- `ox-rss'. Unfortunately, it works by publishing
;; *each file* as RSS, with the convention that top-level entries map
;; to RSS feed items. This is not what I want, since I write blog
;; posts on a one-entry-per-file basis.

;; I found an interesting workaround to this: setup an RSS project for
;; the blog that uses the sitemap feature to create a .org file
;; describing all posts that is suitable for export to RSS. Then wrap
;; the RSS export function to skip RSS export for all files other than
;; the sitemap file.

(defun indie-org-publish-to-rss (plist filename pub-dir)
  "Publish RSS 2.0 with PLIST, only when FILENAME is the sitemap file.
PUB-DIR is when the output will be placed."
  (let ((sitemap-filename (plist-get plist :sitemap-filename)))
    (if (equal sitemap-filename (file-name-nondirectory filename))
        (org-rss-publish-to-rss plist filename pub-dir))))

(defun indie-org--format-rss-entry (entry style project)
  "Format ENTRY for the RSS feed.

ENTRY is a file name.  STYLE is either 'list' or 'tree'.  PROJECT
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

(defun indie-org--format-rss-sitemap (title list)
  "Generate the RSS sitemap file.
TITLE shall be the RSS 2.0 title.  LIST shall be a representation
of the files and directories involved in the project as a nested
list."
  (concat "#+TITLE: " title "\n"
   (org-list-to-subtree list 0 (list :icount "" :istart ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          h-feed support                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-publish-to-h-feed (plist filename pub-dir)
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

(defun indie-org--format-entry-for-h-feed (entry style project)
  "Format ENTRY for an h-feed.

ENTRY is a file name.  STYLE is either 'list' or 'tree'.  PROJECT
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
            (insert (format "* [[file:%s][%s]]\n" entry title))
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

(defun indie-org--format-h-feed-sitemap (title list)
  "Generate h-feed, as a string.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.

It can further be transformed using `org-list-to-generic',
`org-list-to-subtree' and alike.  See also
`org-publish-sitemap-default'.

Suitable for use as an argument to :sitemap-function"
  (concat "#+TITLE: " title "\n"
          (org-list-to-subtree list 0 (list :icount "" :istart ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                  publication state support                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             overview                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Supporting various Indieweb features such as Webmentions requires
;; maintaining state regarding site publication. IOW, every time one
;; publishes the site (whether to prod, staging, or somewhere else)
;; one now needs to scribble-down certain information for subsequent
;; use.

;; As per usual, I maintain this information as a LISP S-expression,
;; serialize it via it's printed representation and deserialize it via
;; `read'. The serialized S-expression at the top level is a property
;; list:

;;     (:version <serialization format version>
;;      :state (
;;        :last-published <last published timestamps>
;;        :webmentions-made <web mentions made>
;;        :webmentions-sent <web mentions sent>
;;        :webmentions-received <web mentions received>
;;        :posse-requests <posse requests>
;;        :posse-responses <posse responses>
;;        :syndication-links <syndication links))

;; Throughout state is indexed by an "environment".  For my personal
;; site, this is :prod (the live site) or :staging (a local copy I use
;; for testing).  These are arbitrary & user-defined.

;; The exceptions are :webmentions-received and :posse-responses,
;; which can only exist for :prod.

;; Another concept used throughout is that of a "page key"; from the
;; perspective of publication state, this need merely be any
;; identifier which can be derived from a page and that uniquely
;; identifies it within the site.  In practice, this should be the
;; relative path of the page as published.

;; Next is the concept of "most recent update".  Org mode defines the
;; :date property for each document.  `indie-org' interprets that as
;; "publication date", and goes on to add a new property :updated,
;; which is interpreted as the most recent update of the page.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    details of each property:                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                    serialization format version                  ;;

;; serialization format version :: this is the version number for the
;; serialization format to ease its evolution; I just use an integer.

(defconst indie-org-pub-state-serde-current-format 1
  "The most recent publication state serialization format.")

;;                          last published                          ;;

;; last published timestamp :: a property list mapping environment to
;; a Lisp timestamp at which the site was last published to that
;; environment; e.g. '(:staging (25261 61056 485925 324000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           webmentions                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                         webmentions made                         ;;

;; webmentions-made :: a property list mapping environment to a hash
;; table mapping page keys to a second hash table which in turn maps
;; update time to webmention targets on that page

;; Schematically:

;;     '(:prod
;;         <hash: page-key :=> <hash: pub/update time :=> (mention...)>>
;;       :staging
;;         <hash: page-key :=> <hash: pub/update time :=> (mention...)>>
;;       ...)

;; NB: There is no need to encode the precise nature of the webmention
;; (i.e. wehther it's a reply, a like, or whatever) here; that
;; information is encoded into the page itself where it can be
;; discovered by the recipient.

;;                         webmentions sent                         ;;

;; webmentions-sent :: a property list mapping environemnt to a hash
;; table mapping page keys to a second hash table which in turn maps
;; webmention targets to a list of `indie-org-sent-wm' instances
;; representing webmentions sent for that (source, target) pair.

;; There may exist multiple webmentions for a given (source, target)
;; pair for a few reasons: the source was updated, for instance.

;; It may seem surprising to maintain a separate collection for each
;; environment-- why not just one, like :webmentions-received. The
;; reason I do this is to support different ways of "sending"
;; webmentions on a per-environment basis: telegraph.io for prod,
;; and "dry-run" for staging, e.g.

;; schematically:

;;     '(:prod
;;         <hash: page-key :=> <hash: mention :=> (wm-sent ...)>>
;;       :staging
;;         <hash: page-key :=> <hash: mention :=> (wm-sent ...)>>
;;       ...)

(cl-defstruct (indie-org-sent-wm
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                indie-org-make-sent-wm
                (&key source target time-sent status kind
                      &aux
                      (_
                       (unless (and source (stringp source))
                         (error "Source shall be a string (%s)" source))
                       (unless (and target (stringp target))
                         (error "Target shall be a string (%s)" target))
                       (unless (and time-sent (listp time-sent))
                         (error "Sent time shall be a Lisp timestamp (%s)" time-sent))))))
  "Sent webmention. SOURCE is the URL of the sending page, TARGET
that of the recipient.  TIME-SENT is a Lisp timestamp
representing the time at which the webmention was sent.  STATUS
is an optional status attribute (for telegraph.io, e.g., it could
be the location header from the response).  KIND is the
webmention kind (generic mention, reply, and so forth) as a
keyword."
  source target time-sent status kind)

;;                       webmentions received                       ;;

;; webmentions-received :: a property list containing :last-checked
;; (the last time webmentions were checked), :last-id (the most-recent
;; webmention ID received), and :mentions: a hash table mapping page
;; key to a list of `indie-org-received-wm' instances representing
;; received webmentions.  Note that unlike other attributes,
;; webmentions-received is *not* keyed by environment; it is assumed
;; that you are using indie-org with a single domain and that these
;; correspond to actual webmentions recieved "in the wild"

(cl-defstruct (indie-org-received-wm-author
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               (:constructor
                indie-org-make-received-wm-author
                (&key name photo type url)))
  "Author of a received webmention."
  name photo type url)

(cl-defstruct (indie-org-received-wm-content
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               (:constructor
                indie-org-make-received-wm-content
                (&key html text)))
  "Received webmention content."
  html text)

(cl-defstruct (indie-org-received-wm
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; LATER(sp1ff): I'm staying loose on the validation
               ;; until I beter understand the requirements on this
               ;; type
               (:constructor
                indie-org-make-received-wm
                (&key id sort time-received source target author
                      content private)))
  "Received webmention."
  id sort time-received source target author content private)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              POSSE                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;                          posse requests                          ;;

;; posse-requests: a property list mapping environment to a hash table
;; mapping page keys to a list of POSSE targets for that page.
;; Targets are the POSSE keywords :twitter, :mastodon &c.

;; schematically:

;;     '(:prod
;;         <hash: page-key :=> (target...)>
;;       :staging
;;         <hash: page-key :=> (target...)>
;;       ...)

;;                         posse responses                          ;;

;; posse-responses: a hash table mapping page keys to a list of
;; `indie-org-posse-response' instances.  Similar to
;; :webmentions-received, this is not a property list keyed by
;; environment.  If a post has been POSSE'd, then it's been POSSE'd,
;; regardless of where we're publishing.

(cl-defstruct (indie-org-posse-response
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                indie-org-make-posse-response
                (&key sort created-at id text url
                      &aux
                      (_
                       (unless (memq sort '(:twitter :mastodon :reddit))
                         (error "Unknown POSSE target %s" sort))
                       (unless (and created-at (stringp created-at))
                         (error "Created-at shall be a string"))
                       (unless (and id (stringp id))
                         (error "Id shall be a string"))
                       (unless (and url (stringp url))
                         (error "URL shall be a string"))))))
  "Response from brid.gy.  SORT is a keyword naming the
silo (:twitter, :mastodon & so on).  CREATED-AT is a Lisp
timestamp reprensenting the time at which the siloed entity was
created, ID it's silo-defined identifer, TEXT the text comprising
the siloed entity and URL it's, well, URL."
  sort created-at id text url)

;;                        syndication links                         ;;

;; syndication links: a property list mapping environment to a hash
;; table mapping page keys to syndicated copies of this post

(cl-defstruct (indie-org-syndicated-copy
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                indie-org-make-syndicated-copy
                (&key silo url type id
                      &aux
                      (_
                       (unless (memq silo '(:twitter :mastodon))
                         (error "%s not supported" silo))))))
  "A syncidated copy of a post"
  silo url type id)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     publication state serde                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-make-publication-state ()
  "Create a fresh, uninitialized copy of publication state."
  (list
   :last-published nil
   :webmentions-made nil
   :webmentions-sent nil
   :webmentions-received
   (list :last-checked nil :last-id nil
         :mentions (make-hash-table :test 'equal))
   :posse-requests nil
   :posse-responses nil
   :syndication-links nil))

(defun indie-org-read-publication-state (filename)
  "Read FILENAME into a plist.

This will read the site publication state file & return the :state portion."
  (let* ((plist
          (car
           (read-from-string
            (with-temp-buffer
              (insert-file-contents filename)
              (buffer-string)))))
         (version (plist-get plist :version)))
    (if (not (eq version indie-org-pub-state-serde-current-format))
        (error "Unknown (or missing) publication state format version: %s" version))
    (plist-get plist :state)))

(defun indie-org--write-sexp-to-file (sexp file-name &optional preamble)
  "Write SEXP to FILE-NAME with optional PREAMBLE.

This is a utility function for persisting LISP S-expressions to
file.  If possible, it will write SEXP to a temporary file in the
same directory as FILE-NAME and then rename the temporary file to
replace the original.  Since the move operation is usually atomic
\(so long as both the source & the target are on the same volume)
any error leaves the original untouched, and there is never any
instant where the file is nonexistent.

This implementation will first check that the target file is
writable and signal an error if it is not.  Note that it will not
attempt to make an existing file writable temporarily.

It will then check that there is only one hard link to it.  If
FILE-NAME has more than one name, then this implementation falls
back to writing directly to the target file.

Finally, it will write SEXP to a temporary file in the same
directory and then rename it to FILENAME.  This implementation
was copied from `elfeed-score-rule-stats--sexp-to-file' which is
itself heavily derivative of `basic-save-buffer-2'."

  (if (not (file-writable-p file-name))
	    (let ((dir (file-name-directory file-name)))
	      (if (not (file-directory-p dir))
	          (if (file-exists-p dir)
		            (error "%s is not a directory" dir)
		          (error "%s: no such directory" dir))
	        (if (not (file-exists-p file-name))
		          (error "Directory %s write-protected" dir)
            (error "Attempt to save to a file that you aren't allowed to write")))))
  (if (not
       (and
        (file-exists-p file-name)
        (> (file-nlinks file-name) 1)))
      ;; We're good-- write to temp file & rename
      (let* ((dir (file-name-directory file-name))
             (tempname
              (make-temp-file
			         (expand-file-name "tmp" dir))))
        (write-region
         (format
          "%s%s"
          (or preamble "")
          (let ((print-level nil)
                (print-length nil))
            (pp-to-string sexp)))
         nil tempname nil nil file-name)
        (rename-file tempname file-name t))
    ;; We're not good-- fall back to writing directly.
    (write-region
         (format
          "%s%s"
          (or preamble "")
          (let ((print-level nil)
                (print-length nil))
            (pp-to-string sexp)))
         nil file-name)))

(defun indie-org-write-publication-state (state file-name)
  "Write STATE to FILE-NAME."
  (indie-org--write-sexp-to-file
   (list
    :version indie-org-pub-state-serde-current-format
    :state state)
   file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          last published                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-get-last-published (state &optional env)
  "Retrieve the last-published time in STATE for ENV."
  (let ((env (or env :prod)))
    (plist-get (plist-get state :last-published) env)))

(defun indie-org-update-last-published (state time &optional env)
  "Update the last-published time in STATE to TIME for ENV.

Returns the new state."
  (let* ((env (or env :prod))
         (new-last-pub
          (plist-put
           (plist-get state :last-published)
           env time)))
    (plist-put state :last-published new-last-pub)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         webmentions made                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-get-webmentions-made (state &optional env)
  "Retrieve the :webmentions-made hash table from STATE.
ENV is the environment of interest.

If none, return a new hash table.  Since the caller can't know
whether the returned hash table is new or not, he should always
call `indie-org-set-webmentions-made' after updating, to ensure
that any changes make it into the publication state."
  (let ((env (or env :prod)))
    (or (plist-get (plist-get state :webmentions-made) env)
        (make-hash-table :test 'equal))))

(defun indie-org-update-webmentions-made (page time mentions sub-state)
  "Update SUB-STATE with WEBMENTIONS for PAGE at TIME.

TIME is the publication date for PAGE, expressed as a Lisp
timestamp.  SUB-STATE shall be a hash table mapping page keys to
a hash table mapping publication timestamps to lists of
webmentions:

    <hash: page :=> <hash <lisp timestamp> :=> (mention...)>>

IOW, the caller needs to pull the hash table for the current
environemnt out of the :webmentions-sent property.  MENTIONS shall
be a list of URLs mentioned in PAGE.

This method will incorporate the new mentions into the hashmap
under PAGE.  Returns nil.

If the current timestamp in MENTIONS matches a timestamp already
in SUB-STATE, just overwrite that entry."
  (let ((previous-mentions (gethash page sub-state)))
    (unless previous-mentions
      (setq previous-mentions (make-hash-table :test 'equal))
      (puthash page previous-mentions sub-state))
    (puthash time mentions previous-mentions)
    nil))

(defun indie-org-set-webmentions-made (state hash &optional env)
  "Update the webmentions-made hash table for ENV.
STATE is the publication state.  HASH is a hash table.

Returns the new publication state."
  (let* ((env (or env :prod))
         (plist (plist-get state :webmentions-made)))
    (setq plist (plist-put plist env hash))
    (plist-put state :webmentions-made plist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         webmentions sent                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-get-webmentions-sent (state &optional env)
  "Retrieve the :webmentions-sent hash table from STATE.
ENV is the environment of interest.

If none, return a new hash table.  Since the caller can't know
whether the returned hash table is new or not, he should always
call `indie-org-set-webmentions-made' after updating, to ensure
that any changes make it into the publication state."
  (let ((env (or env :prod)))
    (or (plist-get (plist-get state :webmentions-sent) env)
        (make-hash-table :test 'equal))))

(defun indie-org-required-webmentions (mentions-made mentions-sent)
  "Determine which mentions still need to be sent.
MENTIONS-MADE is the :webmentions-made hash table for the current
environment.
MENTIONS-SENT is the :webmentions-sent hash for the same.

In more detail, MENTIONS-MADE is a hash table mapping page-key to
a hash table mapping publication time to a list of targets.

MENTIONS-SENT is a hash table mapping page-key to a hash table
mapping target to a list of `indie-org-sent-wm'.

This method will compare the two & determine which, if any,
webmentions still need to be sent.  Returns a list of cons cells,
each of which represents a webmention that should be sent.  The
car will be the page key & the cdr the target."
  ;; For each page in `mentions-made'
  ;; 1. pull the one or two (if possible) most recent publications
  ;; 2. form the union of the mentions made in those 1 or 2 publicatinos
  ;; 3. pull the list of :webmentions-sent for that page
  ;; 4. for each URL in 2.
  ;;    1) look it up in 3
  ;;    2) if the corresponding tiemstamp from 1 is greater than that
  ;;       in 3 (or its just not there), then we need to send a WM
  (let ((mentions-to-send))
    ;; For each page-key in MENTIONS-MADE...
    (maphash
     (lambda (page-key publications-table)
       ;; PUBLICATIONS-TABLE is a hash table mapping publication time to
       ;; (URL...).  We seek the two most recent publications.  We have
       ;; a hash table mapping publication-timestamp to mentions.  Let's
       ;; turn that into a list...
       (let ((publications))
         (maphash
          (lambda (pub-time mentions-made)
            (setq
             publications
             (cons
              (cons pub-time mentions-made)
              publications)))
          publications-table)
         ;; `publications' is now a list of lists.  Each sub-list is of
         ;; the form:
         ;;     (<pub-timestamp> mention...)
         ;; Let's sort it:
         (sort
          publications
          (lambda (lhs rhs) (> (float-time (car lhs)) (float-time (car rhs)))))
         ;; Now, `publications' looks like:
         ;;     ((<timestamp> mention...) (<timestamp> mention...)...)
         ;; sorted in descending order.  But it could be empty, have
         ;; only one sub-list, or have two or more sub-lists.
         ;; Regardless, we need the set union of all such mentions.
         (let ((pub-time (caar publications))
               (recent-mentions
                (cond
                 ((eq (length publications) 0)
                  nil)
                 ((eq (length publications) 1)
                  (cdar publications))
                 (t
                  (let ((first (cdr (nth 0 publications)))
                        (second (cdr (nth 1 publications))))
                    (cl-union first second :test 'equal))))))
           (let ((sent-mentions (gethash page-key mentions-sent)))
             ;; `sent-mentions' may be nil, if we've never sent any webmentions for this page!
             ;; Otherwise, it will be a hash from target to a list of `indie-org-sent-wm'.
             ;; `recent-mentions' is a list of all the webmention
             ;; targets we need to ensure have been, or will be, hit.
             (while recent-mentions
               (if sent-mentions
                   ;; (let ((last-sent (gethash (car recent-mentions) sent-mentions)))
                   ;;   (unless (and last-sent (< (float-time pub-time) (float-time last-sent)))
                   ;;     (setq mentions-to-send (cons (cons page-key (car recent-mentions)) mentions-to-send))))
                   (let* ((target (car recent-mentions))
                          (wms
                           (sort
                            (gethash target sent-mentions)
                            (lambda (lhs rhs)
                              (>
                               (float-time (indie-org-sent-wm-time-sent lhs))
                               (float-time (indie-org-sent-wm-time-sent rhs))))))
                          (last-sent
                           (if (car wms)
                               (indie-org-sent-wm-time-sent (car wms))
                             nil)))
                     (unless (and last-sent (< (float-time pub-time) (float-time last-sent)))
                       (setq mentions-to-send (cons (cons page-key (car recent-mentions)) mentions-to-send))))
                   ;; `sent' is nil; therefore we have to send a webmention
                   (setq mentions-to-send (cons (cons page-key (car recent-mentions)) mentions-to-send)))
               (setq recent-mentions (cdr recent-mentions)))))))
     mentions-made)
    mentions-to-send))

(defun indie-org-record-sent-webmention (mention time sub-state
                                                 &optional status kind)
  "Record a sent webmention.
MENTION shall be a cons cell (SOURCE . TARGET).
TIME is the Lisp timestamp at which the webmention was sent.
SUB-STATE shall be the :webmentions-sent hash table for the
salient environment.
STATUS is an optional status attribute (for telegraph.io, it could be the
location header for the response).
KIND is the webmention kind (generic mention, reply, and so forth).

SUB-STATE should be a hash table mapping page-key to a hash table
mapping target to a list of `indie-org-sent-wm'."
  (let* ((source (car mention))
         (dest (cdr mention))
         (sent-wm (indie-org-make-sent-wm :source source
                                          :target dest
                                          :time-sent time
                                          :status status
                                          :kind kind))
         (source-sents (gethash source sub-state)))
    ;; `source-sents' is a hash from target to list of sent wm-s. May
    ;; be `nil'.
    (if source-sents
        (puthash dest (cons sent-wm (gethash dest source-sents)) source-sents)
      (let ((source-sents (make-hash-table :test 'equal)))
        (puthash dest (list sent-wm) source-sents)
        (puthash source source-sents sub-state)))))

(defun indie-org-set-webmentions-sent (state hash &optional env)
  "Update the webmentions-sent hash table for ENV.
STATE is the :webmentions-sent property list.  HASH is a
hash table mapping page key to inner hash.

Returns the new publication state."
  (let* ((env (or env :prod))
         (plist (plist-get state :webmentions-sent)))
    (setq plist (plist-put plist env hash))
    (plist-put state :webmentions-sent plist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       webmentions received                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-get-webmentions-received (state)
  "Retrieve the :webmentions-received plist from STATE."
  (plist-get state :webmentions-received))

(defun indie-org-update-webmentions-received (state webmentions-received)
  "Update STATE with WEBMENTIONS-RECEIVED.

Returns the new publication state."
  (plist-put state :webmentions-received webmentions-received))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          posse requests                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-get-posse-requests (state &optional env)
  "Retrieve the :posse-requests for ENV from STATE.
STATE is the publication state.

If none, return a new hash table.  Since the caller can't know
whether the returned hash table is new or not, he should always
call `indie-org-set-webmentions-made' after updating, to ensure
that any changes make it into the publication state."
  (let ((env (or env :prod))
        (pr (plist-get state :posse-requests)))
    (or (and pr (plist-get pr env))
        (make-hash-table :test 'equal))))

(defun indie-org-set-posse-requests (state hash &optional env)
  "Update the posse-requests hash table for ENV.
STATE is the current publication state.  HASH is a hash table.

Returns the new publication state."
  (let* ((env (or env :prod))
         (plist (plist-get state :posse-requests)))
    (setq plist (plist-put plist env hash))
    (plist-put state :posse-requests plist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         posse responses                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-get-posse-responses (state)
  "Retrieve the :posse-responses from STATE.
STATE is the publication state.

If none, return a new hash table.  Since the caller can't know
whether the returned hash table is new or not, he should always
call `indie-org-set-webmentions-made' after updating, to ensure
that any changes make it into the publication state."
  (or (plist-get state :posse-responses)
      (make-hash-table :test 'equal)))

(defun indie-org-required-posses (requests responses)
  "Determine the set of POSSE requests to be made.
REQUESTS shall be a hash from page key a list of POSSE
targets ('twitter, 'mastodon, and so on).  RESPONSES shall be
collection of previously completed POSSE requests, in the form of
a hash table mapping page keys to lists of
`indie-org-posse-response' instances.

Return a list of cons cells, each of whose car is a page key
and whose cdr is a list of POSSE symbols."
  (let ((results))
    ;; Walk REQUESTS...
    (maphash
     ;; and for each page key/list of POSSE targets...
     (lambda (page-key required)
       ;; build a list of POSSE responses
       (let* ((resp
               (mapcar
                #'indie-org-posse-response-sort
                (gethash page-key responses)))
              (needed
               (cl-set-difference
                required
                resp
                :test 'equal)))
         (if needed (setq results (cons (cons page-key needed) results)))))
     requests)
    results))

(defun indie-org-record-sent-posse (page-key response posse-responses)
  "Record a POSSE RESPOSNE to PAGE-KEY.
POSSE-RESPONSES shall be a hash of page-key to lists of
previously received respones."
  (let ((curr (gethash page-key posse-responses)))
    (if (not (member response curr))
        (puthash
         page-key
         (cons response curr)
         posse-responses))))

(defun indie-org-set-posse-responses (state hash)
  "Update the posse-requests hash table.
STATE is the publication state.  HASH is a hash table matching a
list of `indie-org-posse-response' instances.

Returns the new publication state."
  (plist-put state :posse-responses hash))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                support for receiving webmentions                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-webmentions-for-page-key (info webmentions-received)
  "Return a list of indie-org-received-webmention instances for a page.
Presumably called during publication.  INFO is a plist used as a
communications channel.  WEBMENTIONS-RECEIVED is a hash table matching
page key to webmentions."
  (gethash
   (indie-org-get-page-key info)
   (plist-get webmentions-received :mentions)))

;; <https://github.com/aaronpk/webmention.io/blob/45a06629e59d56efdba1ce39936e61b81fc92d97/helpers/formats.rb#L169>
(defun indie-org-string-to-wm-sort (text)
  "Convert the 'wm-property' string TEXT returned from webmention.io to keyword."
  (cond
   ((string= text "mention-of") :mention)
   ((string= text "in-reply-to") :reply)
   ((string= text "repost-of") :repost)
   ((string= text "like-of") :like)
   ((string= text "bookmark-of") :bookmark)
   (t (error "Unknown webmention type %s" text))))

(defun indie-org-wm-sort-to-verb (sort)
  "Convert a mention type keyword SORT to a verb."
  (cond
   ((eq sort :mention) "mentioned")
   ((eq sort :reply) "replied to")
   ((eq sort :repost) "reposted")
   ((eq sort :like) "liked")
   ((eq sort :bookmark) "bookmarked")
   (t (error "Unknown webmention type %s" sort))))

(defun indie-org-check-webmentions (domain token state)
  "Check for new webmentions for DOMAIN.
TOKEN shall be your webmention.io token.

STATE shall be the property list corresponding to
:webmentions-received in the publication state.  IOW it itself
shall be a property list with the following fields:

    - :last-checked :: Lisp timestamp representing the last time
      received webmentions were checked
    - :last-id :: integer ID of the last webmention received
    - :mentions :: hash table mapping page keys to lists of
      `indie-org-received-wm' instances for each page

Return the new plist."
  (let* ((last-id (plist-get state :last-id))
         (request-params
          (let ((init (list (cons "domain" domain) (cons "token" token))))
            (if last-id
                (cons (cons "since_id" last-id) init)
              init)))
         (mentions (plist-get state :mentions))
         (rsp))
    (if last-id
        (message "Requesting webmentions more recent than ID %d..." last-id)
      (message "Requesting all webmentions..."))
    (request "https://webmention.io/api/mentions.jf2"
      :params request-params
      :sync t
      :parser (lambda ()
                (let ((json-object-type 'hash-table))
                  (json-read)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (setq rsp error-thrown)))
      :success (cl-function
	              (lambda (&key data &allow-other-keys)
	                (setq rsp data))))
    (unless (hash-table-p rsp)
      (error "Failed to retrieve webmentions: %s" rsp))
    (let ((entries (gethash "children" rsp))) ;; an array or nil
      (if (and entries (arrayp entries))
          (let ((num-entries (length entries)))
            (if last-id
                (message
                 "Requesting webmentions more recent than ID %d...done(got %d \
mentions)."
                 last-id num-entries)
              (message "Requesting all webmentions...done(got %d mentions)."
                       num-entries))
            (message "Processing %d webmentions..." num-entries)
            (cl-loop
             for i from 0 to (1- num-entries)
             do
             (let* ((entry (aref entries i))
                    (author-hash (gethash "author" entry))
                    (author
                     (indie-org-make-received-wm-author
                      :name (gethash "name" author-hash)
                      :photo (gethash "photo" author-hash)
                      :type (gethash "type" author-hash)
                      :url (gethash "url" author-hash)))
                    (content-hash (gethash "content" entry))
                    (content
                     (indie-org-make-received-wm-content
                      :html (gethash "html" content-hash)
                      :text (gethash "text" content-hash)))
                    (id (gethash "wm-id" entry))
                    (target (gethash "wm-target" entry))
                    (wm
                     (indie-org-make-received-wm
                      :id id
                      :sort (indie-org-string-to-wm-sort (gethash "wm-property" entry))
                      :time-received
                      (encode-time (parse-time-string (gethash "wm-received" entry)))
                      :source (gethash "wm-source" entry)
                      :target target
                      :author author
                      :content content
                      :private (gethash "wm-private" entry)))
                    (domain-with-authority (concat "https://" domain))
                    (page-key
                     (and (string-prefix-p domain-with-authority target)
                          (substring target (1+ (length domain-with-authority)))))
                    (wms (gethash page-key mentions)))
               ;; `wms' is a list of `indie-org-received-wm'
               (unless
                   (cl-find
                    wm wms
                    :test
                    (lambda (lhs rhs)
                      (eq
                       (indie-org-received-wm-id lhs)
                       (indie-org-received-wm-id rhs))))
                 (puthash page-key (cons wm wms) mentions)
                 (setq last-id (max id (or last-id 0)))))
             finally
             (message "Processing %d webmentions...done(last-id %d)."
                      num-entries (or last-id 0)))
            (setq state (plist-put state :last-id last-id))
            (setq state (plist-put state :last-checked (current-time))))))
    state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 support for sending webmentions                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Webmentions (AKA WM) shall be sent:

;; 1. on publication of a new post

;; 2. on update of a post, in which case they shall be sent to anyone
;;    mentioned in the previous version as well as anyone mentioned in
;;    the new update

;; My scheme is to record in the publishing state file, for each page,
;; each publication/update and the mentions corresponding to each.

;; WM come in a few flavors: reply, like, repost, or just a mention
;; <https://indieweb.org/webmention-implementation-details#types_of_mentions>.
;; Furthermore, I don't want to send a WM for *every* link in my
;; posts; I want it to be intentional.

;; For this reason, I introduce new Org link types to model this
;; <https://orgmode.org/manual/Adding-Hyperlink-Types.html>. This will
;; let me:

;; 1. express my intent in the original Org document; when I want to
;;    note that a particular link is not just a link, but a mention, I
;;    can say: [[mention:https://foo.com/bar][bar]]

;; 2. get my fingers into each such link at export time, through the
;;    export function that is part of the link definition; I take this
;;    opportunity to update a property in the "communications channel"
;;    that maps to a list of Webmentions (for sending, the type
;;    doesn't matter-- it's up to the recipient to parse the HTML to
;;    see what sort of WM it is).

;; Next, I (ab)use the `final-output' filter function to update
;; variable within the comms channel with the accumulated WM since, at
;; this point, I know the page permalink and all links have been
;; processed. I just accumulate WM until the entire project is
;; published, at which point I commit it all to disk.

(defun indie-org-record-webmention (link info)
  "Note the fact that the current page mentions LINK in INFO.

Update the communications channel property :indie-org/mentions with LINK."
  (let ((mentions (plist-get info :indie-org/mentions)))
    (plist-put info :indie-org/mentions (cons link mentions))))

(defun indie-org-browse-mention (path _)
  "Follow a mention link PATH."
  (browse-url path))

(defun indie-org-export-mention (link description backend info)
  "Export a generic webmention.
LINK & DESCRIPTION are as for any Org link.
BACKEND shall be the Org Export backend.
INFO is a plist used as a communications channel during export.

Link export functions are invoked with:

  - the path, as a string
  - the description as a string, or nil
  - the export back-end
  - the export communication channel, as a plist"
  (when (org-export-derived-backend-p backend 'html)
    (indie-org-record-webmention link info)
    (format "<a href=\"%s\">%s</a>" link description)))

(defun indie-org-export-reply (link description backend info)
  "Export a reply webmention LINK/DESCRIPTION for backend BACKEND with INFO."
  (when (org-export-derived-backend-p backend 'html)
    (indie-org-record-webmention link info)
    (format "<a href=\"%s\" class=\"u-in-reply-to\">%s</a>" link description)))

(defun indie-org-export-like (link description backend info)
  "Export a like webmention LINK/DESCRIPTION for backend BACKEND with INFO."
  (when (org-export-derived-backend-p backend 'html)
    (indie-org-record-webmention link info)
    (format "<a href=\"%s\" class=\"u-like-of\">%s</a>" link description)))

(defun indie-org-export-repost (link description backend info)
  "Export a repost webmention LINK/DESCRIPTION for backend BACKEND with INFO."
  (when (org-export-derived-backend-p backend 'html)
    (indie-org-record-webmention link info)
    (format "<a href=\"%s\" class=\"u-repost-of\">%s</a>" link description)))

;; OK: at this point, the webmentions are in :indie-org/mentions. Next
;; steps:

;; 1. grab DATE out of the parse tree (it's an error if it's not present)
;; 2. possibly grab UPDATED (may not be there)
;; 3. write-down the *page* :=> date, webmentions

(defun indie-org-finalize-page (backend info)
  "Collect all webmentions & update state.
BACKEND is the Org export backend.
INFO is a plist used as a communications channel.

Callers should arrange to call this function after each page is
exported (the final-output filter would be a good place).  It will
grab all webmentions from the :indie-org/mentions INFO property and
update the publication state."
  (when (org-export-derived-backend-p backend 'indie-org)
    (let* ((mentions (plist-get info :indie-org/mentions))
           (page-key (indie-org-get-page-key info))
           (webmentions-made (plist-get info :indie-org/webmentions-made))
           (input-file (plist-get info :input-file))
           (publish-date
            (org-publish-find-date input-file info))
           (updated
            (let ((val
                   (org-publish-find-property input-file :updated info 'indie-org)))
              (if val
                  (org-time-string-to-time
                   (org-element-interpret-data
                    val))))))
      ;; `mentions' is just a list of URLs I've mentioned on this page.
      (indie-org-update-webmentions-made
       page-key
       (or updated publish-date)
       mentions
       webmentions-made))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          POSSE support                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-send-webmention (wm token)
  "Send a Webmention via telegraph.io.
WM shall be a cons cell whose car is the source page & whose cdr
is the target.
TOKEN is the telegraph.io API token."
  (let (rsp)
    (request "https://telegraph.p3k.io/webmention"
      :type "POST"
      :sync t
      :data (list (cons "source" (car wm))
                  (cons "target" (cdr wm))
                  (cons "token" token))
      :parser #'json-read
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (error "While sending webmention %s :=> %s, got %s"
                       (car wm) (cdr wm) rsp)))
      :success (cl-function
	              (lambda (&key data &allow-other-keys)
	                (setq rsp data))))
    ;; `rsp' should be an alist with properties 'status and 'location
    (message "%s :=> %s (%s)." (car wm) (cdr wm) rsp)
    (alist-get 'location rsp)))

(defun indie-org-send-posse-request (source posse-target)
  "Send a POSSE request to brid.gy.
SOURCE is the page to be published (i.e. its full URL).
POSSE-TARGET is one of :twitter or :mastodon.  TOKEN is my brid.gy token.

Note that SOURCE will need to have a (possibly empty) link to the
brid.gy webmention endpoint corresponding to target on the page
somewhere for the webmention we're about to send to be
accepted (<a href=\"https://brid.gy/publish/twitter\"></a>, e.g.).

Return an indie-org-posse-response."
  (let ((target (indie-org-posse-target-to-wm-target posse-target))
        (rsp))
    (request "https://brid.gy/publish/webmention"
      :type "POST"
      :sync t
      :data (list (cons "source" source)
                  (cons "target" target))
      :parser #'json-read
      :complete (cl-function
                 (lambda (&key data error-thrown symbol-status response &allow-other-keys)
                   (cond
                    ((eq symbol-status 'success)
                     (setq rsp
                           (indie-org-make-posse-response
                            :sort posse-target
                            :created-at (alist-get 'created_at data)
                            :id (alist-get 'id data)
                            :text (alist-get 'text data)
                            :url (alist-get 'url data))))
                    ((eq symbol-status 'error)
                     (message "While sending POSSE request %s :=> %s, got:" source target)
                     (message "    data: %s" data)
                     (message "  symbol: %s" symbol-status)
                     (message "response: %s" error-thrown)
                     (let* ((original (alist-get 'original data)))
                       (if original
                           (setq rsp (indie-org-make-posse-response
                                      :sort posse-target
                                      :created-at (alist-get 'created_at original)
                                      :id (alist-get 'id original)
                                      :text (alist-get 'text original)
                                      :url (alist-get 'url original)))
                         (error "Unrecognized error!"))))
                    (t
                     (message "While sending POSSE request %s :=> %s, got:" source target)
                     (message "    data: %s" data)
                     (message "  symbol: %s" symbol-status)
                     (message "response: %s" error-thrown)
                     ;; symbol-status: one of success/error/timeout/abort/parse-error
                     (error "Unexpected result while sending POSSE request: %s" symbol-status))))))
    rsp))

(defun indie-org-string-to-posse-target (text)
  "Convert a #+POSSE value TEXT to keyword."
  (cond
   ((string= text "twitter") :twitter)
   ((string= text "mastodon") :mastodon)
   ((string= text "github") :github)
   ((string= text "flickr") :flickr)
   ((string= text "reddit") :reddit)
   (t (error "Unknown POSSE target %s" text))))

(defun indie-org-posse-target-to-wm-target (target)
  "Convert a POSSE keyword to the corresponding webmention target at brid.gy.
TARGET shall be a keyword (:twitter, e.g.)"
  (concat
   "https://brid.gy/publish/"
   (cond
    ((eq target :twitter) "twitter")
    ((eq target :mastodon) "mastodon")
    ((eq target :reddit) "reddit")
    ((eq target :github) "github")
    ((eq target :flickr) "flickr")
    (t (error "Unknown POSSE target %s" target)))))

(defun indie-org-posse-target-to-string (target)
  "Convert a POSSE TARGET keyword to a human-friendly string."
  (cond
    ((eq target :twitter) "Twitter")
    ((eq target :mastodon) "Mastodon")
    ((eq target :github) "Github")
    ((eq target :flickr) "Flickr")
    ((eq target :reddit) "Reddit")
    (t (error "Unknown POSSE target %s" target))))

(defun indie-org-record-posse-request (posse-targets page-key posse-requests)
  "Record one or more POSSE targets.
POSSE-TARGETS is a space-delimited list of POSSE
targets ('twitter, 'mastodon, and so forth).
PAGE-KEY is the key naming the page to be POSSE'd.
POSSE-REQUESTS is a hash table mapping page-key to POSSE requests."
  (puthash
   page-key
   (mapcar #'indie-org-string-to-posse-target (split-string posse-targets))
   posse-requests))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         public functions                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-get-page-key (info)
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

(defun indie-org-read-token (filename)
  "Read a token from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (string-trim (buffer-string))))

(defun indie-org-plain-text (contents info)
  "Convert plain text into html encoded text.
CONTENTS shall be Org-down.
INFO is a plist used as a communications channel.

Shamelessly copied from `ox-rss.el'"
  (let (output)
    (setq output (org-html-encode-plain-text contents)
	        output (org-export-activate-smart-quotes
		              output :html info))))

(defun indie-org-find-posts (project-dir exclude-drafts &rest kwargs)
  "Determine which posts in PROJECT-DIR are to be included.

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
                          dirent :draft pseudo-project 'indie-org))))
           (setq out (cons dirent out))))
     (directory-files project-dir nil ".*\\.org$"))
    out))

(defun indie-org-enable-rss-2.0-feed (project sitemap-filename feed-title feed-description)
  "Add an RSS 2.0 feed to an Org Export PROJECT.

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
    (setq plist (plist-put plist :publishing-function #'indie-org-publish-to-rss))
    (setq plist (plist-put plist :sitemap-filename sitemap-filename))
    (setq plist (plist-put plist :sitemap-format-entry #'indie-org--format-rss-entry))
    (setq plist (plist-put plist :sitemap-sort-files 'anti-chronologically))
    (setq plist (plist-put plist :sitemap-function #'indie-org--format-rss-sitemap))
    (setq plist (plist-put plist :feed-title feed-title))
    (setq plist (plist-put plist :description feed-description))
    (cons name plist)))

(defun indie-org-enable-full-h-feed (project sitemap-filename feed-title feed-description backend)
  "Add a full h-feed to an Org Export PROJECT.

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
    (setq plist (plist-put plist :publishing-function #'indie-org-publish-to-h-feed))
    (setq plist (plist-put plist :sitemap-format-entry #'indie-org--format-entry-for-h-feed))
    (setq plist (plist-put plist :sitemap-sort-files 'anti-chronologically))
    (setq plist (plist-put plist :sitemap-function #'indie-org--format-h-feed-sitemap))
    (setq plist (plist-put plist :hfeed-name feed-title))
    (setq plist (plist-put plist :description feed-description))
    (setq plist (plist-put plist :h-feed-backend backend))
    (setq plist (plist-put plist :with-toc nil))
    (cons name plist)))

;;;###autoload
(defun indie-org-enable ()
  "Enable indie-org'.

Define a new export backend (derived from HTML) that will add
DRAFT & UPDATED keywords."

  ;; It would be nice if there was way to "un-define" these backends &
  ;; "un-set" these link parameters so as to enable an
  ;; `indie-org-unload'.
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
                     (:posse "POSSE" nil nil space)))
  (org-export-define-derived-backend 'indie-org-h-feed 'html
    ;; h-feed core properties
    :options-alist
    '((:hfeed-name "HFEED_NAME" nil nil newline)
      (:hfeed-url "HFEED_URL" nil nil t)
      (:hfeed-photo "HFEED_PHOTO" nil t)))

  (org-link-set-parameters
   "mention"
   :follow #'indie-org-browse-mention
   :export #'indie-org-export-mention)
  (org-link-set-parameters
   "reply"
   :follow #'indie-org-browse-mention
   :export #'indie-org-export-reply)
  (org-link-set-parameters
   "like"
   :follow #'indie-org-browse-mention
   :export #'indie-org-export-like)
  (org-link-set-parameters
   "repost"
   :follow #'indie-org-browse-mention
   :export #'indie-org-export-repost))

(defun indie-org--pp-last-published (publication-state)
  "Pretty-print the last-published information from PUBLICATION-STATE."
  (message "last-published:")
  (let ((last-published (plist-get publication-state :last-published)))
    (while last-published
      (let ((env (car last-published))
            (time (cadr last-published)))
        (message "  %s: %s" env (format-time-string "%Y-%m-%d %H:%M:%S" time))
        (setq last-published (cddr last-published))))))

(defun indie-org--pp-webmentions-received (publication-state)
  "Pretty-print the webmentions received in PUBLICATION-STATE."
  (let* ((webmentions-received
          (indie-org-get-webmentions-received publication-state))
         (mentions
          (plist-get webmentions-received :mentions)))
    (message "webmentions received:")
    (let ((last-checked (plist-get webmentions-received :last-checked))
          (last-id (plist-get webmentions-received :last-id)))
      (if last-checked
          (message
           "  last-checked: %s"
           (format-time-string
            "%Y-%m-%d %H:%M:%S"
            last-checked)))
      (if last-id
          (message
           "  last-id: %d"
           last-id)))
    (message "  received:")
    (maphash
     (lambda (page-key wms)
       (when wms
         (message "    %s received:" page-key)
         ;; `wms' is a list of `indie-org-receive-wm' instances
         (while wms
           (let ((wm (car wms)))
             (message "      %s: %s"
                      (format-time-string "%Y-%m-%d %H:%M:%S"
                                          (indie-org-received-wm-time-received wm))
                      (indie-org-received-wm-source wm)))
           (setq wms (cdr wms)))))
     mentions)))

(defun indie-org--pp-webmentions-made (publication-state)
  "Pretty-print the webmentions made in PUBLICATION-STATE."
  (message "webmentions made:")
  (let ((webmentions-made (plist-get publication-state :webmentions-made)))
    (while webmentions-made
      (let ((env (car webmentions-made))
            (hash (cadr webmentions-made)))
        (message "  %s:" env)
        (maphash
         (lambda (page-key mentions)
           ;; `mentions' is a hash table: <hash: pub-date :=> (mention...)>
           (maphash
            (lambda (pub-date mentions)
              (when (not (null mentions))
                (message
                 "    %s (%s, %d mentions):"
                 page-key
                 (format-time-string "%Y-%m-%d %H:%M:%S" pub-date)
                 (length mentions))
                (while mentions
                  (message "      %s" (car mentions))
                  (setq mentions (cdr mentions)))))
            mentions))
         hash))
      (setq webmentions-made (cddr webmentions-made)))))

(defun indie-org--pp-posse-requests (publication-state)
  "Pretty-print the POSSE requests in PUBLICATION-STATE."
  (message "POSSE requests:")
  (let ((posse-requests (plist-get publication-state :posse-requests)))
    (while posse-requests
      (let ((env (car posse-requests))
            (hash (cadr posse-requests)))
        (message "    %s:" env)
        (maphash
         (lambda (page-key targets)
           (message "        %s:" page-key)
           (while targets
             (message "            %s" (car targets))
             (setq targets (cdr targets))))
         hash))
      (setq posse-requests (cddr posse-requests)))))

(defun indie-org--pp-webmentions-sent (publication-state)
  "Pretty-print the webmentions sent in PUBLICATION-STATE."
  (message "webmentions sent:")
  (let ((webmentions-sent (plist-get publication-state :webmentions-sent)))
    (while webmentions-sent
      (let ((env (car webmentions-sent))
            (hash (cadr webmentions-sent)))
        (message "  %s:" env)
        (maphash
         (lambda (page-key hash)
           ;; `hash' is <hash: pub-date => (mention...)
           (when (not (eq 0 (hash-table-count hash)))
             (message "    %s:" page-key)
             (maphash
              (lambda (target mentions)
                (when (not (null mentions))
                  (message "      %s" target)
                  (while mentions
                    (let ((wm (car mentions)))
                      (message "        %s: %s"
                               (format-time-string "%Y-%m-%d %H:%M:%S" (indie-org-sent-wm-time-sent wm))
                               (indie-org-sent-wm-status wm)))
                    (setq mentions (cdr mentions)))))
              hash)))
         hash))
      (setq webmentions-sent (cddr webmentions-sent)))))

(defun indie-org--pp-posse-responses (publication-state)
  "Pretty-print the POSSE responses in PUBLICATION-STATE."
  (message "POSSE responses:")
    (let ((posse-responses (plist-get publication-state :posse-responses)))
      (if posse-responses
          (maphash
           (lambda (page-key responses)
             (message "    %s:" page-key)
             (while responses
               (let ((rsp (car responses)))
                 (message "        %s:" (indie-org-posse-response-sort rsp))
                 (message "            %s" (indie-org-posse-response-created-at rsp))
                 (message "            %s" (indie-org-posse-response-id rsp))
                 (message "            %s" (indie-org-posse-response-url rsp)))
               (setq responses (cdr responses))))
           posse-responses))))

(defun indie-org-pp-state (publication-state)
  "Pretty-print the publication state in PUBLICATION-STATE."
  (indie-org--pp-last-published publication-state)
  (indie-org--pp-webmentions-received publication-state)
  (indie-org--pp-webmentions-made publication-state)
  (indie-org--pp-posse-requests publication-state)
  (indie-org--pp-webmentions-sent publication-state)
  (indie-org--pp-posse-responses publication-state))

(provide 'indie-org)
;;; indie-org.el ends here

