;;; indie-org-state.el -- indie-org publication state  -*- lexical-binding: t -*-

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

;; Supporting various Indieweb features such as Webmentions & POSSE requires
;; maintaining state regarding site publication.  For instance, every time one
;; publishes the site (whether to prod, staging, or somewhere else) one now
;; needs to scribble down new webmentions that need to be sent for subsequent
;; use.  Incoming webmentions also need to be written down upon receipt (for
;; potential use in subsequent publications).

;; This package defines that state, as well as providing a serde mechanism.
;; The goal is to have web sites built on `indie-org' be able to add site-
;; specific state via the CL struct :include keyword; e.g.

;;     (require 'indie-org-state)
;;     (cl-defstruct (my-site-state (:include indie-org-state) new-state))

;; (cf. <https://www.gnu.org/software/emacs/manual/html_mono/cl#Structures>)

;; indie-org also supports the idea of "publication environments"; separate,
;; parallel publication targets for your site.  These are meant to be defined
;; on a site-by-site basis & named by keywords.  For instance, one might want a
;; development, staging & production copy of one's site (and one might name
;; them `:dev', `:stg' & `:prod').

;; To continue, the indie-org publication state serialization format for the
;; above example site would be something like:

;;     (list
;;         :version indie-org-state-current-format
;;         (list
;;             :prod <printed repr of `my-site-state' for the production site>
;;             :stg <printed repr of `my-site-state' for the staging site>
;;             :dev <printed repr of `my-site-state' for the dev site>))

;; The serde mechanism allows for versioning by tagging the serialized state
;; with a version tag.  On read, older versions can still be deserialized and
;; converted to the current format.  If the site has defined its own custom
;; state, it will have to provide deserialization methods to provide this
;; feature.

;; In particular, the 0.3 & 0.4 builds of `indie-org' significantly changed the
;; state representation.  Versions 0.1 & 0.2 used an archaic state
;; representation.  Version 0.3 used the print form of the types used in the
;; implementation for serde, but the author quickly realized that this made
;; updating structs inconvenient & so switched to the present approach of
;; converting structs to property lists & using _their_ print form.

;;; Code:
(require 'cl-lib)
(require 'indie-org-webmentions)
(require 'indie-org-posse)

(defconst indie-org-state-serde-current-format 3
  "The most recent publication state serialization format.")

(defvar indie-org-state-fallbacks (make-hash-table))

(cl-defstruct (indie-org-state
               (:constructor nil)
               (:constructor
                indie-org-make-publication-state))
  "This struct is deprecated & will be removed in a subsequent
release. Use `indie-org-state-v2' instead."
  (last-published nil :type list
                  :documentation "The Lisp timestamp of the most recent publication")
  (webmentions-made nil :type indie-org-webmentions-made)
  (webmentions-sent nil :type indie-org-webmentions-sent)
  (webmentions-received nil :type indie-org-webmentions-received)
  (posse-requests nil :type indie-org-posse-requests)
  (posse-responses nil :type indie-org-posse-responses)
  (syndication-links nil :type indie-org-posse-syndicated-copies))

(make-obsolete 'indie-org-state 'indie-org-state-v2 "0.3")

(cl-defstruct (indie-org-state-v2
               (:constructor nil)
               (:constructor
                indie-org-state-make))
  "Site state for a given publication environment as maintained by indie-org.

This struct maintains collections of webmentions received & POSSE
responses for each publication environment.  That may seem
surprising, but this opens-up the possibility of sending
webmentions or POSSE requests differently in different
environments (webmentin.io & brid.gy for production, mocks for
dev, e.g.)"
  (last-published nil :type list :documentation "The Lisp timestamp of the most recent publication")
  (webmentions-made nil :type indie-org-webmentions-made)
  (webmentions-sent nil :type indie-org-webmentions-sent)
  (webmentions-received nil :type indie-org-webmentions-received)
  (posse-requests nil :type indie-org-posse-requests)
  (posse-responses nil :type indie-org-posse-responses))

(defun indie-org-state-v2-from-v1 (old-state)
  "Convert OLD-STATE from an `indie-or-state' to an `indie-org-state-v2'."
  ;; It's mostly a matter of just copying over old members, while dropping
  ;; syndicated copies...
  (indie-org-state-make
   :last-published       (indie-org-state-last-published       old-state)
   :webmentions-made     (indie-org-state-webmentions-made     old-state)
   :webmentions-sent     (indie-org-state-webmentions-sent     old-state)
   :webmentions-received (indie-org-state-webmentions-received old-state)
   :posse-requests       (indie-org-state-posse-requests       old-state)
   :posse-responses
   (indie-org-posse-responses-v2-from-v1
    (indie-org-state-posse-responses old-state))))

(defun indie-org-state-get-last-published (state &optional env)
  "Retrieve the last-published time in STATE for ENV.
STATE shall be a property list mapping publication environment
names to either `indie-org-state-v2' structures or structs that
subtype `indie-org-state-v2'."
  (let ((env (or env :prod)))
    (indie-org-state-v2-last-published (plist-get state env))))

(defun indie-org-state-update-last-published (state time &optional env)
  "Update the last-published time in STATE to TIME for ENV."
  (let* ((env (or env :prod)))
    (setf (indie-org-state-v2-last-published (plist-get state env)) time)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          publication state serde                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-state--plist-to-state (plist)
  "Interpret PLIST as an `indie-org-state-v2 instance.

Internal.  Parse the input property list as the current
publication state representation."

  (indie-org-state-make
   :last-published (plist-get plist :last-published)
   :webmentions-made     (indie-org-webmentions-made-from-plist     (plist-get plist :webmentions-made))
   :webmentions-sent     (indie-org-webmentions-sent-from-plist     (plist-get plist :webmentions-sent))
   :webmentions-received (indie-org-webmentions-received-from-plist (plist-get plist :webmentions-received))
   :posse-requests       (indie-org-posse-requests-from-plist       (plist-get plist :posse-requests))
   :posse-responses      (indie-org-posse-responses-from-plist      (plist-get plist :posse-responses))))

(defun indie-org-state--state-to-plist (state)
  "Serialize STATE to a property list."

  (list
   :last-published       (indie-org-state-v2-last-published state)
   :webmentions-made     (indie-org-webmentions-made-to-plist     (indie-org-state-v2-webmentions-made     state))
   :webmentions-sent     (indie-org-webmentions-sent-to-plist     (indie-org-state-v2-webmentions-sent     state))
   :webmentions-received (indie-org-webmentions-received-to-plist (indie-org-state-v2-webmentions-received state))
   :posse-requests       (indie-org-posse-requests-to-plist       (indie-org-state-v2-posse-requests       state))
   :posse-responses      (indie-org-posse-responses-to-plist      (indie-org-state-v2-posse-responses      state))))

(defun indie-org-state-read (filename &optional format-version fallbacks)
  "Read FILENAME into a plist mapping publication environment to state.
FORMAT-VERSION is the current serialization format version number.
FALLBACKS is a hash table mapping prior format versions to deserializers.

This will read the site publication state file & return a plist
mapping publication environment names to `indie-org-state-v2'
instances.  If FILENAME is in an archaic format (which is to say:
its version is less than `indie-org-state-serde-current-format'),
FILENAME will be upgraded to the current format.  A backup will
be left in the same directory, and the file will be re-written in
the most recent format."

  ;; We expect the print form for a property list with two properties:
  ;; `:version' and `:state'; this is consistent across all supported serde
  ;; format versions.  What has varied has been the value of the `:state'
  ;; property... how we map that to a plist from publication environment name
  ;; to `indie-org-state-v2' varies depending on the `:version'.
  (let* ((plist
          (car
           (read-from-string
            (with-temp-buffer
              (insert-file-contents filename)
              (buffer-string)))))
         (version (plist-get plist :version))
         (state
          (if (eq version (or format-version indie-org-state-serde-current-format))
              (cl-loop for (key value) on (plist-get plist :state) by 'cddr append
                       (list key (indie-org-state--plist-to-state value)))
            (let ((f (gethash version (or fallbacks indie-org-state-fallbacks))))
              (if f
                  (funcall f plist)
                (error "Unknown (or missing) publication state format version: %s" version))))))
    state))

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

(defun indie-org-state-write (state file-name &optional format-version)
  "Write STATE to FILE-NAME with optional serialization format FORMAT-VERSION.

STATE shall be a property list mapping publication environment
keywords to `indie-org-publication-state' instances."

  (unless (listp state)
    (signal 'wrong-type-argument (list #'listp state)))
  (indie-org--write-sexp-to-file
   (list
    :version (or format-version indie-org-state-serde-current-format)
    :state
    (cl-loop for (key value) on state by 'cddr append
             (list key (indie-org-state--state-to-plist value))))
   file-name))

(defun indie-org-state-v2-fallback (sexp)
  "Interpret SEXP as a v2 serialization form."
  (let ((old-state (plist-get sexp :state)))
    (cl-loop for (key value) on old-state by 'cddr append
             (list key (indie-org-state-v2-from-v1 value)))))

(defun indie-org-state-v1-fallback (sexp)
  "Interpret SEXP as a v1 serialization format."
  (let ((old-state (plist-get sexp :state))
        ;; 👇 `hash' is a hash table mapping pub env name to `indie-org-state' instance
        (hash (make-hash-table))
        (plist))
    (while old-state
      (let ((key (car old-state))
            (value (cadr old-state)))
        (cond
         ((eq key :last-published)
          ;; `value' 👇 is a plist mapping pub env to timestamp
          (while value
            (let* ((env (car value))
                   (time (cadr value))
                   (state (or (gethash env hash) (indie-org-state-make))))
              (setf (indie-org-state-v2-last-published state) time)
              (puthash env state hash))
            (setq value (cddr value))))
         ((eq key :webmentions-made)
          ;; `value' 👇 is a plist mapping pub env to hash table; each hash
          ;; table maps page-key to a second hash table The second hash table
          ;; maps timestamp to webmention target.
          (while value
            (let* ((env (car value))
                   (hash1 (cadr value))
                   (state (or (gethash env hash) (indie-org-state-make)))
                   (made (or (indie-org-state-v2-webmentions-made state) (indie-org-webmentions-make-made))))
              ;; Iterate over `hash1'-- keys are page-keys and values are hash tables.
              (maphash
               (lambda (page-key hash2)
                 ;; Iterate over `hash2'-- keys are update times, values are
                 ;; lists of URLs
                 (maphash
                  (lambda (update-time mentions)
                    (indie-org-webmentions-update-made made page-key update-time mentions))
                  hash2))
               hash1)
              (setf (indie-org-state-v2-webmentions-made state) made)
              (puthash env state hash))
            (setq value (cddr value))))
         ((eq key :webmentions-sent)
          ;; `value' is a plist mapping pub env to hash table; each hash table
          ;; maps page keys to another hash table. The inner hash table maps
          ;; webmention targets on that page to lists of `indie-org-sent-wm'.
          (while value
            (let* ((env (car value))
                   (hash1 (cadr value))
                   (state (or (gethash env hash) (indie-org-state-make)))
                   (sent (or (indie-org-state-v2-webmentions-sent state) (indie-org-webmentions-make-sent))))
              ;; Iterate over `hash1'-- keys are page keys and values are hash
              ;; tables.
              (maphash
               (lambda (page-key hash2)
                 ;; Iterate over `hash2'-- keys are webmentions targets, and
                 ;; values are lists of `indie-org-sent-wm' instances.
                 (maphash
                  (lambda (target sent-wms)
                    (while sent-wms
                      (let* ((this-send (car sent-wms))
                             (sent-wm (indie-org-webmentions-make-sent-wm
                                       :source (indie-org-sent-wm-source this-send)
                                       :target (indie-org-sent-wm-target this-send)
                                       :time-sent (indie-org-sent-wm-time-sent this-send)
                                       :status (indie-org-sent-wm-status this-send)
                                       :kind (indie-org-sent-wm-kind this-send))))
                        ;; OK-- we've converted the archaic sent-wm to the modern sent-wm.
                        (indie-org-webmentions-update-sent sent page-key target sent-wm))
                      (setq sent-wms (cdr sent-wms))))
                  hash2))
               hash1)
              (setf (indie-org-state-v2-webmentions-sent state) sent)
              (puthash env state hash))
            (setq value (cddr value))))
         ((eq key :webmentions-received)
          ;; `value' is a property list whose properties correspond to the fields of
          ;; `indie-org-webmentions-received'.  Unlike other fields, there is only
          ;; *one* plist (that for :prod).
          (let* ((last-checked (plist-get value :last-checked))
                 (last-id (plist-get value :last-id))
                 (mentions (plist-get value :mentions)))
            ;; The problem is that mentions is a hash table mapping page-keys
            ;; to lists of `indie-org-received-wm' instances, which in turn
            ;; contain `indie-org-received-wm-author' and
            ;; `indie-org-received-wm-author' instances, so we need to build-up
            ;; an `indie-org-webmentions-received-wm' one-by-one.
            (let ((state (or (gethash :prod hash) (indie-org-state-make)))
                  (recvd (indie-org-webmentions-make-received)))
              (setf (indie-org-webmentions-received-last-checked recvd) last-checked)
              (setf (indie-org-webmentions-received-last-id recvd) last-id)
              (maphash
               (lambda (page-key received-wms)
                 (while received-wms
                   (let* ((old-received-wm (car received-wms))
                          (old-author (indie-org-received-wm-author old-received-wm))
                          (author
                           (if old-author
                               (indie-org-webmentions--make-received-wm-author
                                :name (indie-org-received-wm-author-name old-author)
                                :photo (indie-org-received-wm-author-photo old-author)
                                :type (indie-org-received-wm-author-type old-author)
                                :url (indie-org-received-wm-author-url old-author))))
                          (old-content (indie-org-received-wm-content old-received-wm))
                          (content
                           (if old-content
                               (indie-org-webmentions--make-received-wm-content
                                :html (indie-org-received-wm-content-html old-content)
                                :text (indie-org-received-wm-content-text old-content))))
                          (received-wm (indie-org-webmentions-make-received-wm
                                        :id (indie-org-received-wm-id old-received-wm)
                                        :sort (indie-org-received-wm-sort old-received-wm)
                                        :time-received (indie-org-received-wm-time-received old-received-wm)
                                        :source (indie-org-received-wm-source old-received-wm)
                                        :target (indie-org-received-wm-target old-received-wm)
                                        :author author
                                        :content content
                                        :private (indie-org-received-wm-private old-received-wm))))
                     ;; Ok-- we've got page-key & a modern "wm-received"
                     (indie-org-webmentions-add-received-wm recvd page-key received-wm))
                   (setq received-wms (cdr received-wms))))
               mentions)
              (setf (indie-org-state-v2-webmentions-received state) recvd)
              (puthash :prod state hash))))
         ((eq key :syndication-links)
          ;; pass
          )
         ((eq key :posse-requests)
          ;; `value' is a plist mapping pub env to hash table. The hash
          ;; tables map page-keys to lists of posse target keywords.
          (while value
            (let* ((env (car value))
                   (hash1 (cadr value))
                   (state (or (gethash env hash) (indie-org-state-make)))
                   (requests (or (indie-org-state-v2-posse-requests state) (indie-org-posse-make-requests))))
              ;; Iterate over `hash1'
              (maphash
               (lambda (page-key old-requests)
                 (puthash page-key old-requests (indie-org-posse-requests-hash requests)))
               hash1)
              (setf (indie-org-state-v2-posse-requests state) requests)
              (puthash env state hash))
            (setq value (cddr value))))
         ((eq key :posse-responses)
          ;; `value' is a hash table mapping page keys to lists of
          ;; `indie-org-posse-response' for :prod.
          (let* ((state (or (gethash :prod hash) (indie-org-state-make)))
                 (rsps (or (indie-org-state-v2-posse-responses state) (indie-org-posse-make-responses))))
            (maphash
             (lambda (page-key old-rsps)
               (while old-rsps
                 (let* ((old-rsp (car old-rsps))
                        ;; Convert `old-rsp' to and `indie-org-posse-resposne'
                        (new-rsp
                         (indie-org-posse-make-response-v2
                          :sort (indie-org-posse-response-sort old-rsp)
                          :created-at (indie-org-posse-response-created-at old-rsp)
                          :id (indie-org-posse-response-id old-rsp)
                          :text (indie-org-posse-response-text old-rsp)
                          :url (indie-org-posse-response-url old-rsp))))
                   (indie-org-record-sent-posse page-key new-rsp rsps))
                 (setq old-rsps (cdr old-rsps))))
             value)
            (setf (indie-org-state-v2-posse-responses state) rsps)
            (puthash :prod state hash)))))
      (setq old-state (cddr old-state)))
    ;; Convert `hash' into a property list
    (maphash
     (lambda (key value)
       (setq plist (cons value plist)
             plist (cons key plist)))
     hash)
    plist))

;;;###autoload
(defun indie-org-state-enable ()
  "Initialize the indie-org-state package."
  (puthash 1 #'indie-org-state-v1-fallback indie-org-state-fallbacks)
  (puthash 2 #'indie-org-state-v2-fallback indie-org-state-fallbacks))

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

(make-obsolete 'indie-org-sent-wm "No longer used" "0.3")

(cl-defstruct (indie-org-received-wm-author
;; Disable the default ctor (the name violates Emacs
;; package naming conventions)
               (:constructor nil)
               (:constructor
                indie-org-make-received-wm-author
                (&key name photo type url)))
  "Author of a received webmention."
  name photo type url)

(make-obsolete 'indie-org-received-wm-author "No longer used" "0.3")

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
               ;; I'm staying loose on the validation until I beter
               ;; understand the requirements on this type
               (:constructor
                indie-org-make-received-wm
                (&key id sort time-received source target author
                      content private)))
  "Received webmention. CONTENT may be nil."
  id sort time-received source target author content private)

(make-obsolete 'indie-org-received-wm "No longer used" "0.3")

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
                (&key silo _url _type _id
                      &aux
                      (_
                       (unless (memq silo '(:twitter :mastodon))
                         (error "%s not supported" silo))))))
  "A syncidated copy of a post"
  )

(make-obsolete 'indie-org-syndicated-copy "No longer used" "0.3")

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

(make-obsolete 'indie-org-posse-response "No longer used" "0.3")

(defun indie-org-state--pp-state (env state)
  "Pretty-print publication state STATE for environment ENV."
  (let ((wm-made (indie-org-state-v2-webmentions-made state))
        (wm-sent (indie-org-state-v2-webmentions-sent state))
        (wm-recv (indie-org-state-v2-webmentions-received state))
        (posse-req (indie-org-state-v2-posse-requests state))
        (posse-rsp (indie-org-state-v2-posse-responses state)))
    (message "%s:\n    Last published: %s"
             env
             (format-time-string
              "%Y-%m-%d %H:%M:%S"
              (indie-org-state-v2-last-published state)))
    (and wm-made (indie-org-webmentions-pp-made wm-made 1))
    (and wm-sent (indie-org-webmentions-pp-sent wm-sent 1))
    (and wm-recv (indie-org-webmentions-pp-received wm-recv 1))
    (and posse-req (indie-org-posse-pp-requests posse-req 1))
    (and posse-rsp (indie-org-posse-pp-responses posse-rsp 1))))

(defun indie-org-state-pp (publication-state)
  "Pretty-print the publication state in PUBLICATION-STATE."
  (while publication-state
    (let ((env (car publication-state))
          (state (cadr publication-state)))
      (indie-org-state--pp-state env state))
    (setq publication-state (cddr publication-state))))

(provide 'indie-org-state)
;;; indie-org-state.el ends here
