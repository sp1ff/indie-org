;;; indie-org-webmentions.el -- Webmentions support  -*- lexical-binding: t -*-

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

;; This package provides Webmention support (sending & receiving) for indie-org sites.

;;; Code:
(require 'indie-org-page-key)
(require 'indie-org-serde)
(require 'request)
(require 'ol)
(require 'ox)
(require 'cl-lib)


(defun indie-org-webmentions--sorted-hash-time-keys (hash)
  "Extract keys from HASH, sort them lexicographically.
Return the resulting list."
  (let ((keys))
    (maphash (lambda (key _val) (setq keys (cons key keys))) hash)
    (sort keys #'time-less-p)))

(defun indie-org-webmentions--sorted-hash-string-keys (hash)
  "Extract keys from HASH, sort them lexicographically.
Return the resulting list."
  (let ((keys))
    (maphash (lambda (key _val) (setq keys (cons key keys))) hash)
    (sort keys #'string<)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          making mention of other sites (i.e. sending Webmentions)         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl-defstruct (indie-org-webmentions-targets
               (:constructor nil)
               (:constructor indie-org-webmentions-make-pub-targets))
  "A mapping from publication time to a list of webmention
targets.

See also `indie-org-webmentions-made'.  This struct encodes the
webmentions made for a particular page across time:

    publication/update time :=> (target...)

Where:

    update-time: is a Lisp timestamp

    target: is just a string containing the URL of the Webmention
    target post"
  ;; `hash' is a hash table from time :=> (target...), where `target' is just a
  ;; string containing an URL
  (hash (make-hash-table :test 'equal) :type hash-table))

(defun indie-org-webmentions-pp-targets (targets &optional indent)
  "Pretty-print pub targets TARGETS at indent level INDENT."
  (unless (indie-org-webmentions-targets-p targets)
    (signal 'wrong-type-argument (list #'indie-org-webmentions-targets-p targets)))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s))
         (indent2 (make-string (+ 4 (* indent 4)) ?\s))
         (hash (indie-org-webmentions-targets-hash targets))
         (keys (indie-org-webmentions--sorted-hash-time-keys hash)))
    (while keys
      (let* ((key (car keys))
             (targets (gethash key hash)))
        (when targets
            (message "%s%s:" indent1 (format-time-string "%Y-%m-%d %H:%M:%S" key))
            (while targets
              (message "%s%s" indent2 (car targets))
              (setq targets (cdr targets)))))
      (setq keys (cdr keys)))))

(defun indie-org-webmentions-targets-to-plist (targets)
    "Serialize TARGETS to a plist."
    (unless (indie-org-webmentions-targets-p targets)
      (signal 'wrong-type-argument (list #'indie-org-webmentions-targets-p targets)))
    (list
     :hash
     (indie-org-serde-hash-to-plist
      (indie-org-webmentions-targets-hash targets))))

(defun indie-org-webmentions-targets-from-plist (plist)
  "Deserialize PLIST to an `indie-org-webmentions-targets'."
  ;; (let ((hash (make-hash-table :test 'equal)))
  ;;   (while plist
  ;;     (let ((key (car plist))
  ;;           (value (cadr plist)))
  ;;       (puthash key value hash))
  ;;     (setq plist (cddr plist)))
  ;;   (indie-org-webmentions-make-pub-targets :hash hash))
  (indie-org-webmentions-make-pub-targets
   :hash
   (indie-org-serde-plist-to-hash
    (plist-get plist :hash))))

(cl-defstruct (indie-org-webmentions-made
               (:constructor nil)
               (:constructor indie-org-webmentions-make-made))
  "Webmentions made for a publication environment.

By \"making\" a webmention, we mean mentioning another post on
your site with the intent of sending a Webmention thereto.

While webmentions made are defined by page, we still need to
track them by publication environment since different
environments will be updated at different times.  This also
opens-up the possiblity of sending Webmentions differently in
different environments (to telegraph.p3k.io) in `:prod', and to a
local mock in development, e.g.)

NB: There is no need to encode the precise nature of the webmention
(i.e. wehther it's a reply, a like, or whatever) here; that
information is encoded into the page itself where it can be
discovered by the recipient.

Since the Webmention W3C Recommendation
<https://www.w3.org/TR/webmention/> takes into account sending a
Webmention more than once (on update or deletion of the source
post), the data model is somewhat complex:

    page-key :=> publication/update time :=> (target...)

Where:

    page-key: any string that uniquely identifies each page on the site

    update-time: is a Lisp timestamp

    target: is just a string containing the URL of the Webmention
    target post"
  ;; `hash' is a hash table from page-key :=> indie-org-webmentions-targets
  (hash (make-hash-table :test 'equal) :type hash-table))

(defun indie-org-webmentions-made-to-plist (made)
    "Serialize MADE to a property list."
    (list
     :hash
     (indie-org-serde-hash-to-plist
      (indie-org-webmentions-made-hash made)
      :serializer #'indie-org-webmentions-targets-to-plist)))

(defun indie-org-webmentions-made-from-plist (plist)
    "Deserialize PLIST to an `indie-org-webmentions-made' instance."
    (indie-org-webmentions-make-made
     :hash
     (indie-org-serde-plist-to-hash
      (plist-get plist :hash)
      :test 'equal
      :deserializer #'indie-org-webmentions-targets-from-plist)))

(defun indie-org-webmentions-pp-made (made &optional indent)
  "Pretty-print webmentions-made MADE at indent level INDENT."
  (unless (indie-org-webmentions-made-p made)
    (signal 'wrong-type-argument (list #'indie-org-webmentions-made-p made)))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s))
         (indent2 (make-string (+ 4 (* indent 4)) ?\s))
         (hash (indie-org-webmentions-made-hash made))
         (page-keys (indie-org-webmentions--sorted-hash-string-keys hash)))
    (message "%sWebmentions made:" indent1)
    (while page-keys
      (let* ((page-key (car page-keys))
             (pub-targets (gethash page-key hash)))
        (message "%s%s:" indent2 page-key)
        (indie-org-webmentions-pp-targets pub-targets (+ 2 indent)))
      (setq page-keys (cdr page-keys)))))

(defun indie-org-webmentions-update-targets (targets time mentions)
  "Insert MENTIONS at TIME into TARGETS."
  (unless (indie-org-webmentions-targets-p targets)
    (signal 'wrong-type-argument (list 'indie-org-webmentions-targets-p targets)))
  (puthash time mentions (indie-org-webmentions-targets-hash targets)))

(defun indie-org-webmentions-update-made (made page time mentions)
  "Update MADE with MENTIONS for PAGE at TIME.

TIME is the publication date for PAGE, expressed as a Lisp
timestamp.  MADE shall be an `indie-org-webmentions-made'
instance.  MENTIONS shall be a list of URLs mentioned in PAGE.

This method will incorporate the new mentions into the hashmap
under PAGE.  Returns nil.

If the current timestamp in TIME matches a timestamp already in
MADE, just overwrite that entry."

  (unless (indie-org-webmentions-made-p made)
    (signal 'wrong-type-argument (list 'indie-org-webmentions-made-p made)))
  (let* ((curr-hash (indie-org-webmentions-made-hash made))
         ;; 👇 is an indie-org-webmentions-targets
	       (curr-targets (or (gethash page curr-hash)
			                     (indie-org-webmentions-make-pub-targets))))
    (indie-org-webmentions-update-targets curr-targets time mentions)
    (puthash page curr-targets curr-hash)
    ;; `puthash' will return `curr-targets', which we don't want to expose to
    ;; the caller.
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         tracking sent Webmentions                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (indie-org-webmentions-sent-wm
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                indie-org-webmentions-make-sent-wm
                (&key source target time-sent status kind
                      &aux
                      (_
                       (unless (and source (stringp source))
                         (error "Source shall be a string (%s)" source))
                       (unless (and target (stringp target))
                         (error "Target shall be a string (%s)" target))
                       (unless (and time-sent (listp time-sent))
                         (error "Sent time shall be a Lisp timestamp (%s)" time-sent))))))
  "Sent Webmention.
SOURCE is the URL of the sending page, TARGET that of the
recipient.  TIME-SENT is a Lisp timestamp representing the time
at which the webmention was sent.  STATUS is an optional status
attribute (for telegraph.io, e.g., it could be the location
header from the response).  KIND is the webmention kind (generic
mention, reply, and so forth) as a keyword."
  source target time-sent status kind)

(defun indie-org-webmentions-pp-sent-wm (sent-wm &optional indent)
  "Pretty-print SENT-WM at indent level INDENT."
  (unless (indie-org-webmentions-sent-wm-p sent-wm)
    (signal 'wrong-type-argument (list #'indie-org-webmentions-sent-wm-p sent-wm)))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s))
         (indent2 (make-string (+ 4 (* indent 4)) ?\s)))
    (message
     "%s%s :=> %s\n%sTime: %s\n%sStatus: %s\n%sKind: %s"
     indent1
     (indie-org-webmentions-sent-wm-source sent-wm)
     (indie-org-webmentions-sent-wm-target sent-wm)
     indent2 (format-time-string "%Y-%m-%d %H:%M:%S" (indie-org-webmentions-sent-wm-time-sent sent-wm))
     indent2 (indie-org-webmentions-sent-wm-status sent-wm)
     indent2 (indie-org-webmentions-sent-wm-kind sent-wm))))

(defun indie-org-webmentions-sent-wm-to-plist (sent)
  "Serialize SENT to a property list."
  (list
   :source    (indie-org-webmentions-sent-wm-source    sent)
   :target    (indie-org-webmentions-sent-wm-target    sent)
   :time-sent (indie-org-webmentions-sent-wm-time-sent sent)
   :status    (indie-org-webmentions-sent-wm-status    sent)
   :kind      (indie-org-webmentions-sent-wm-target    sent)))

(defun indie-org-webmentions-sent-wms-to-plist (lyzt)
    "Serializes LYZT to a property list."
    (mapcar #'indie-org-webmentions-sent-wm-to-plist lyzt))

(defun indie-org-webmentions-sent-wm-from-plist (plist)
  "Deserialize PLIST to an `indie-org-webmentions-sent-wm'."
  (indie-org-webmentions-make-sent-wm
   :source    (plist-get plist :source)
   :target    (plist-get plist :target)
   :time-sent (plist-get plist :time-sent)
   :status    (plist-get plist :status)))

(defun indie-org-webmentions-sent-wms-from-plist (lyzt)
  "Deserialize LYZT to a list of property lists."
  (mapcar #'indie-org-webmentions-sent-wm-from-plist lyzt))

(cl-defstruct (indie-org-webmentions-send-targets
               (:constructor nil)
               (:constructor indie-org-webmentions--make-send-targets))
  "All sent Webmentions for a particular target (on a given page).

See also `indie-org-webmentions-sent'.  This struct captures the
mapping:

    target :=> (wm-sent...)

Where:

    target: is just a string containing the URL of the Webmention
    target post

    wm-sent: is an `indie-org-webmentions-sent-wm' instance"
  ;; `hash` is a hash table mapping wm target to to (`indie-org-webmentions-sent-wm'...)
  (hash (make-hash-table :test 'equal) :type hash-table))

(defun indie-org-webmentions-pp-send-targets (targets &optional indent)
  "Pretty-print send targets TARGETS at indent INDENT."
  (unless (indie-org-webmentions-send-targets-p targets)
    (signal 'wrong-type-argument (list #'indie-org-webmentions-send-targets-p targets)))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s))
         (hash (indie-org-webmentions-send-targets-hash targets))
         (targets (indie-org-webmentions--sorted-hash-string-keys hash)))
    (while targets
      (let* ((target (car targets))
             (sent-wms (gethash target hash)))
        (when sent-wms
          (message "%sTarget: %s" indent1 target)
          (while sent-wms
            (indie-org-webmentions-pp-sent-wm (car sent-wms) (1+ indent))
            (setq sent-wms (cdr sent-wms)))))
      (setq targets (cdr targets)))))

(defun indie-org-webmentions-send-targets-to-plist (targets)
  "Serialize TARGETS to a propertly list."
  (unless (indie-org-webmentions-send-targets-p targets)
    (signal 'wrong-type-argument (list #'indie-org-webmentions-send-targets-p targets)))
  (list
   :hash
   (indie-org-serde-hash-to-plist
    (indie-org-webmentions-send-targets-hash targets)
    :serializer #'indie-org-webmentions-sent-wms-to-plist)))

(defun indie-org-webmentions-send-targets-from-plist (plist)
  "Deserialize PLIST to an `indie-org-webmentions-send-targets'."
  (indie-org-webmentions--make-send-targets
   :hash
   (indie-org-serde-plist-to-hash
    (plist-get plist :hash)
    :test #'equal
    :deserializer #'indie-org-webmentions-sent-wms-from-plist)))

(defun indie-org-webmentions-update-send-targets (targets target sent-wm)
  "Update an `indie-org-webmentions-send-targets'.
TARGETS shall be an `indie-org-webmentions-send-targets' instance.
TARGET shall be a string containing the URL of the Webmention target.
SENT-WM shall be an `indie-org-webmentions-sent-wm' instance describing
the sent Webmention."
  (unless (indie-org-webmentions-send-targets-p targets)
    (signal 'wrong-type-argument (list 'indie-org-webmentions-send-targets-p targets)))
  ;; 👇 hash from page-key to a list of sent wms
  (let* ((hash (indie-org-webmentions-send-targets-hash targets))
         (curr-list (gethash target hash)))
    (puthash
     target
     (if curr-list (cons sent-wm curr-list) (list sent-wm))
     hash)))

(cl-defstruct (indie-org-webmentions-sent
               (:constructor nil)
               (:constructor indie-org-webmentions-make-sent))
  "All sent Webmentions for the site for a publication
environment.

Since the Webmention W3C Recommendation
<https://www.w3.org/TR/webmention/> takes into account sending a
Webmention more than once (on update or deletion of the source
post), the data model is somewhat complex:

    page-key :=> target :=> (wm-sent...)

Where:

    page-key: any string that uniquely identifies each page on the site

    target: is just a string containing the URL of the Webmention
    target post

    wm-sent: is an `indie-org-webmentions-sent-wm' instance

It may seem surprising to maintain a separate collection for each
environment-- why not just one? The reason we do this is to
support different ways of \"sending\" webmentions on a
per-environment basis: telegraph.p3k.io for prod, and \"dry-run\"
for staging, e.g."
  ;; `hash` is a hash table mapping page key to to `indie-org-webmentions-send-targets'
  (hash (make-hash-table :test 'equal) :type hash-table))

(defun indie-org-webmentions-sent-to-plist (sent)
  "Serialize SENT to a property list."
  (list
   :hash
   (indie-org-serde-hash-to-plist
    (indie-org-webmentions-sent-hash sent)
    :serializer #'indie-org-webmentions-send-targets-to-plist)))

(defun indie-org-webmentions-sent-from-plist (plist)
  "Deserialize PLIST to an `indie-org-webmentions-send-targets'."
  (unless (listp plist)
    (signal 'wrong-type-argument (list #'listp plist)))
  (indie-org-webmentions-make-sent
   :hash
   (indie-org-serde-plist-to-hash
    (plist-get plist :hash)
    :test #'equal
    :deserializer #'indie-org-webmentions-send-targets-from-plist)))

(defun indie-org-webmentions-pp-sent (sent &optional indent)
  "Pretty-print sent webmentions SENT at indent INDENT."
  (unless (indie-org-webmentions-sent-p sent)
    (signal 'wrong-type-argument (list #'indie-org-webmentions-sent-p sent)))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s))
         (indent2 (make-string (+ 4 (* indent 4)) ?\s))
         (hash (indie-org-webmentions-sent-hash sent))
         (page-keys (indie-org-webmentions--sorted-hash-string-keys hash)))
    (message "%sWebmentions sent:" indent1)
    (while page-keys
      (let ((page-key (car page-keys)))
        (message "%s%s:" indent2 page-key)
        (indie-org-webmentions-pp-send-targets (gethash page-key hash) (1+ indent)))
      (setq page-keys (cdr page-keys)))))

(defun indie-org-webmentions-update-sent (sent page target sent-wm)
  "Record a sent Webmention.
SENT shall be an `indie-org-webmentions-sent' instance.
PAGE shall be the key identifying the post sending the Webmention.
TARGET shall be the URL of the Webmention target.  SENT-WM shall be
an `indie-org-webmentions-sent-wm' instance containing the
status of the sent Webmention."

  (unless (indie-org-webmentions-sent-p sent)
    (signal 'wrong-type-argument (list 'indie-org-webmentions-sent-p sent)))
  (let* ((curr-hash (indie-org-webmentions-sent-hash sent))
         (curr-targets (or (gethash page curr-hash)
                           (indie-org-webmentions--make-send-targets))))
    (indie-org-webmentions-update-send-targets curr-targets target sent-wm)
    (puthash page curr-targets curr-hash)
    nil))

(defun indie-org-webmentions-required (mentions-made mentions-sent)
  "Determine which mentions still need to be sent.
MENTIONS-MADE is an `indie-org-webmentions-made' instance
containing all the Webmentions made for a given publication
environment.
MENTIONS-SENT is an `indie-org-webmentions-sent' instance
containing the list of Webmentions already made for this
publication environment.

In more detail, MENTIONS-MADE is a mapping from page-key to a
second mapping from publication time to a list of targets.

MENTIONS-SENT is a mapping from page-key to a second mapping from
target to a list of `indie-org-sent-wm'.

This method will compare the two & determine which, if any,
Webmentions still need to be sent.

Returns a list of cons cells, each of which represents a
webmention that should be sent.  The car will be the page key &
the cdr the target."
  ;; For each page in `mentions-made'
  ;; 1. pull the one or two (if possible) most recent publications
  ;; 2. form the union of the mentions made in those 1 or 2 publicatinos
  ;; 3. pull the list of :webmentions-sent for that page
  ;; 4. for each URL in 2.
  ;;    1) look it up in 3
  ;;    2) if the corresponding tiemstamp from 1 is greater than that
  ;;       in 3 (or its just not there), then we need to send a WM
  (unless (indie-org-webmentions-made-p mentions-made)
    (signal 'wrong-type-argument (list 'indie-org-webmentions-made-p mentions-made)))
  (unless (indie-org-webmentions-sent-p mentions-sent)
    (signal 'wrong-type-argument (list 'indie-org-webmentions-sent-p mentions-sent)))
  (let ((mentions-to-send)
        (mentions-sent (indie-org-webmentions-sent-hash mentions-sent)))
    ;; For each page-key in MENTIONS-MADE...
    (maphash
     (lambda (page-key publications-table)
       ;; PUBLICATIONS-TABLE is an `indie-org-webmentions-targets'
       ;; instance containing a hash table mapping publication time to
       ;; (URL...).  We seek the two most recent publications.  We have a hash
       ;; table mapping publication-timestamp to mentions.  Let's turn that
       ;; into a list...
       (unless (indie-org-webmentions-targets-p publications-table)
         (signal 'wrong-type-argument (list #'indie-org-webmentions-targets-p publications-table)))
       (let ((publications))
         (maphash
          (lambda (pub-time mentions-made)
            (setq
             publications
             (cons
              (cons pub-time mentions-made)
              publications)))
          (indie-org-webmentions-targets-hash publications-table))
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
             ;; `sent-mentions' may be nil, if we've never sent any webmentions
             ;; for this page!  Otherwise, it will be an
             ;; `indie-org-webmentions-send-targets' instance containing a hash
             ;; from target to a list of `indie-org-sent-wm'.
             (if sent-mentions
                 (setq sent-mentions (indie-org-webmentions-send-targets-hash sent-mentions)))
             ;; `recent-mentions' is a list of all the webmention targets we
             ;; need to ensure have been, or will be, hit.
             (while recent-mentions
               (if sent-mentions
                   (let* ((target (car recent-mentions))
                          (wms
                           (sort
                            (gethash target sent-mentions)
                            (lambda (lhs rhs)
                              (>
                               (float-time (indie-org-webmentions-sent-wm-time-sent lhs))
                               (float-time (indie-org-webmentions-sent-wm-time-sent rhs))))))
                          (last-sent
                           (if (car wms)
                               (indie-org-webmentions-sent-wm-time-sent (car wms))
                             nil)))
                     (unless (and last-sent (< (float-time pub-time) (float-time last-sent)))
                       (setq mentions-to-send (cons (cons page-key (car recent-mentions)) mentions-to-send))))
                 ;; `sent' is nil; therefore we have to send a webmention
                 (setq mentions-to-send (cons (cons page-key (car recent-mentions)) mentions-to-send)))
               (setq recent-mentions (cdr recent-mentions)))))))
     (indie-org-webmentions-made-hash mentions-made))
    mentions-to-send))

(defun indie-org-webmentions-send (wm token)
  "Send a Webmention via telegraph.p3k.io.
WM shall be a cons cell whose car is the source page & whose cdr
is the target.
TOKEN is the telegraph.p3.io API token to be used for authentication."
  (let (rsp)
    (request "https://telegraph.p3k.io/webmention"
      :type "POST"
      :sync t
      :data (list (cons "source" (car wm))
                  (cons "target" (cdr wm))
                  (cons "token" token))
      :parser #'json-read
      :error (cl-function
              (lambda (&key data error-thrown &allow-other-keys)
                (error "While sending webmention %s :=> %s, got %s (%S)"
                       (car wm) (cdr wm) error-thrown data)))
      :success (cl-function
	              (lambda (&key data &allow-other-keys)
	                (setq rsp data))))
    ;; `rsp' should be an alist with properties 'status and 'location
    (message "%s :=> %s (%s)." (car wm) (cdr wm) rsp)
    (alist-get 'location rsp)))

(defun indie-org-webmentions-record-sent (mention time sent &optional status kind)
  "Record a sent webmention.
MENTION shall be a cons cell (SOURCE . TARGET).
TIME is the Lisp timestamp at which the webmention was sent.
SENT shall be the `indie-org-webmentions-sent' instance for
the salient environment.
STATUS is an optional status attribute (for telegraph.io, it could be the
location header for the response).
KIND is the webmention kind (generic mention, reply, and so forth).

Returns nil."
  (let* ((source (car mention))
          (dest (cdr mention))
          (sent-wm (indie-org-webmentions-make-sent-wm :source source
                                                       :target dest
                                                       :time-sent time
                                                       :status status
                                                       :kind kind)))
    (indie-org-webmentions-update-sent sent source dest sent-wm)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           receiving Webmentions                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <https://github.com/aaronpk/webmention.io/blob/45a06629e59d56efdba1ce39936e61b81fc92d97/helpers/formats.rb#L169>
(defun indie-org-webmentions--string-to-wm-sort (text)
  "Convert the `wm-property' string TEXT returned from webmention.io to keyword."
  (cond
   ((string= text "mention-of") :mention)
   ((string= text "in-reply-to") :reply)
   ((string= text "repost-of") :repost)
   ((string= text "like-of") :like)
   ((string= text "bookmark-of") :bookmark)
   (t (error "Unknown webmention type %s" text))))

(defun indie-org-webmentions-wm-sort-to-verb (sort)
  "Convert a mention type keyword SORT to a verb."
  (cond
   ((eq sort :mention) "mentioned")
   ((eq sort :reply) "replied to")
   ((eq sort :repost) "reposted")
   ((eq sort :like) "liked")
   ((eq sort :bookmark) "bookmarked")
   (t (error "Unknown webmention type %s" sort))))

(cl-defstruct (indie-org-webmentions--received-wm-author
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               (:constructor
                indie-org-webmentions--make-received-wm-author
                (&key name photo type url)))
  "Author of a received webmention."
  name photo type url)

(defun indie-org-webmentions-received-wm-author-to-plist (author)
  "Serialize AUTHOR to a property list."
  (list
   :name  (indie-org-webmentions--received-wm-author-name  author)
   :photo (indie-org-webmentions--received-wm-author-photo author)
   :type  (indie-org-webmentions--received-wm-author-type  author)
   :url   (indie-org-webmentions--received-wm-author-url   author)))

(defun indie-org-webmentions-received-wm-author-from-plist (plist)
  "Deserialize PLIST to an `indie-org-webmentions--received-wm-auhtor' instance."
  (indie-org-webmentions--make-received-wm-author
   :name  (plist-get plist :name)
   :photo (plist-get plist :photo)
   :type  (plist-get plist :type)
   :url   (plist-get plist :url)))

(cl-defstruct (indie-org-webmentions--received-wm-content
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               (:constructor
                indie-org-webmentions--make-received-wm-content
                (&key html text)))
  "Received webmention content."
  html text)

(defun indie-org-webmentions-received-wm-content-to-plist (content)
  "Serialize CONTENT to a property list."
  (list
   :html (indie-org-webmentions--received-wm-content-html content)
   :text (indie-org-webmentions--received-wm-content-text content)))

(defun indie-org-webmentions-received-wm-content-from-plist (plist)
  "Deserialize PLIST to an `indie-org-webmentions--received-wm-content' instance."
  (indie-org-webmentions--make-received-wm-content
   :html (plist-get plist :html)
   :text (plist-get plist :text)))

(cl-defstruct (indie-org-webmentions-received-wm
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; I'm staying loose on the validation until I beter
               ;; understand the requirements on this type
               (:constructor
                indie-org-webmentions-make-received-wm
                (&key id sort time-received source target author
                      content private)))
  "Received webmention. CONTENT may be nil."
  id sort time-received source target author content private)

(defun indie-org-webmentions-pp-received-wm (wm &optional indent)
  "Pretty-print a received webmention WM at indent level INDENT."
  (unless (indie-org-webmentions-received-wm-p wm)
    (signal 'wrong-type-argument (list #'indie-org-webmentions-received-wm-p wm)))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s))
         (indent2 (make-string (+ 4 (* indent 4)) ?\s)))
    (message
     "%s%s :=> %s\n%sid: %s\n%ssort: %s\n%sreceived: %s\n%sauthor: %s\n%scontent: %s\n%sprivate: %s"
     indent1
     (indie-org-webmentions-received-wm-source wm)
     (indie-org-webmentions-received-wm-target wm)
     indent2 (indie-org-webmentions-received-wm-id wm)
     indent2 (indie-org-webmentions-received-wm-sort wm)
     indent2
     (format-time-string "%Y-%m-%d %H:%M:%S" (indie-org-webmentions-received-wm-time-received wm))
     indent2 (indie-org-webmentions-received-wm-author wm)
     indent2 (indie-org-webmentions-received-wm-content wm)
     indent2 (indie-org-webmentions-received-wm-private wm))))

(defun indie-org-webmentions-received-wm-to-plist (received-wm)
  "Serialize RECEIVED-WM to a property list."
  (list
   :id            (indie-org-webmentions-received-wm-id            received-wm)
   :sort          (indie-org-webmentions-received-wm-sort          received-wm)
   :time-received (indie-org-webmentions-received-wm-time-received received-wm)
   :source        (indie-org-webmentions-received-wm-source        received-wm)
   :target        (indie-org-webmentions-received-wm-target        received-wm)
   :author        (indie-org-webmentions-received-wm-author-to-plist (indie-org-webmentions-received-wm-author received-wm))
   :content       (indie-org-webmentions-received-wm-content-to-plist (indie-org-webmentions-received-wm-content received-wm))
   :private       (indie-org-webmentions-received-wm-private received-wm)))

(defun indie-org-webmentions-received-wm-from-plist (plist)
  "Deserialize PLIST to an `indie-org-webmentions-received-wm'."
  (indie-org-webmentions-make-received-wm
   :id            (plist-get plist :id)
   :sort          (plist-get plist :sort)
   :time-received (plist-get plist :time-received)
   :source        (plist-get plist :source)
   :target        (plist-get plist :target)
   :author        (indie-org-webmentions-received-wm-author-from-plist (plist-get plist :author))
   :content       (indie-org-webmentions-received-wm-content-from-plist (plist-get plist :content))
   :private       (plist-get plist :private)))

(defun indie-org-webmentions-received-wm-list-to-plist (lzt)
  "Serialize LZT to a list of property lists.

Each property list will represent an `indie-org-webmentions-received-wm'."
  (mapcar #'indie-org-webmentions-received-wm-to-plist lzt))

(defun indie-org-webmentions-received-wm-list-from-plist (lzt)
    "Deserialize LZT to a list of `indie-org-webmentions-received-wm'."
  (mapcar #'indie-org-webmentions-received-wm-from-plist lzt))

(cl-defstruct (indie-org-webmentions-received
               (:constructor nil)
               (:constructor indie-org-webmentions-make-received))
  "All the webmentions received by the site in a given publication environment."
  (last-checked nil :documentation "the last time webmentions were checked")
  (last-id nil :documentation "the most-recent webmention ID received")
  ;; hash: page-key :=> (indie-org-webmentions-received-wm...)
  (mentions (make-hash-table :test 'equal)
            :documentation "hash table mapping page key to a list of `indie-org-webmentions-received-wm'
instances representing received webmentions"))

(defun indie-org-webmentions-received-to-plist (received)
  "Serialize RECEIVED to a property list."
  (if received
      (list
       :last-checked (indie-org-webmentions-received-last-checked received)
       :last-id      (indie-org-webmentions-received-last-id received)
       :mentions     (indie-org-serde-hash-to-plist
                      (indie-org-webmentions-received-mentions received)
                      #'indie-org-webmentions-received-wm-list-to-plist))))

(defun indie-org-webmentions-received-from-plist (plist)
  "Deserialize PLIST to an `indie-org-webmentions-received' instance."
  (indie-org-webmentions-make-received
   :last-checked (plist-get plist :last-checked)
   :last-id      (plist-get plist :last-id)
   :mentions     (indie-org-serde-plist-to-hash
                  (plist-get plist :mentions)
                  :test #'equal
                  :deserializer #'indie-org-webmentions-received-wm-list-from-plist)))

(defun indie-org-webmentions-received-for-page-key (received page-key)
    "Return a list of webmentions-received for a given PAGE-KEY.
RECEIVED is an `indie-org-webmentions-received' instance."
  (unless (indie-org-webmentions-received-p received)
    (signal 'wrong-type-argument (list #'indie-org-webmentions-received-p received)))
  (gethash page-key (indie-org-webmentions-received-mentions received)))

(defun indie-org-webmentions-add-received-wm (received page-key received-wm)
  "Add a new received webmention.
RECEIVED shall be an `indie-org-webmentions-received'.
PAGE-KEY shall be the page for which the webmention was received.
RECEIVED-WM shall be an `indie-org-webmentions-received-wm'
The new webmention shall not be added to the page's list if a webmention with
that ID is already there.
Return the id of this webmention if it was added, nil else."

  (let* ((mentions (indie-org-webmentions-received-mentions received))
         (wms (gethash page-key mentions))
         (id))
    (unless
        (cl-find
         received-wm wms
         :test
         (lambda (lhs rhs)
           (eq
            (indie-org-webmentions-received-wm-id lhs)
            (indie-org-webmentions-received-wm-id rhs))))
      (puthash page-key (cons received-wm wms) mentions)
      (setq id (indie-org-webmentions-received-wm-id received-wm)))
    id))

(defun indie-org-webmentions-pp-received (recvd &optional indent)
  "Pretty-print RECVD at indent level INDENT."
  (unless (indie-org-webmentions-received-p recvd)
    (signal 'wrong-type-argument (list #'indie-org-webmentions-received-p recvd)))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s))
         (indent2 (make-string (+ 4 (* indent 4)) ?\s))
         (hash (indie-org-webmentions-received-mentions recvd))
         (last-id (indie-org-webmentions-received-last-id recvd))
         (page-keys (indie-org-webmentions--sorted-hash-string-keys hash)))
    (message "%sWebmentions received:\n%sLast checked: %s\n%sLast ID: %s"
             indent1
             indent2
             (format-time-string
              "%Y-%m-%d %H:%M:%S"
              (indie-org-webmentions-received-last-checked recvd))
             indent2
             (if last-id (format "%d" last-id) "nil"))
    (while page-keys
      (let* ((page-key (car page-keys))
             (wms (gethash page-key hash)))
        (while wms
          (indie-org-webmentions-pp-received-wm (car wms) (1+ indent))
          (setq wms (cdr wms))))
      (setq page-keys (cdr page-keys)))))

(defun indie-org-webmentions-check (domain token state)
  "Check for new webmentions for DOMAIN.
TOKEN shall be your webmention.io token.  STATE shall be an
`indie-org-webmentions-received'.

Update STATE & return nil."
  (let* ((last-id (indie-org-webmentions-received-last-id state))
         (request-params
          (let ((init (list (cons "domain" domain) (cons "token" token))))
            (if last-id
                (cons (cons "since_id" last-id) init)
              init)))
         (mentions (indie-org-webmentions-received-mentions state))
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
                     (indie-org-webmentions--make-received-wm-author
                      :name (gethash "name" author-hash)
                      :photo (gethash "photo" author-hash)
                      :type (gethash "type" author-hash)
                      :url (gethash "url" author-hash)))
                    (content-hash (gethash "content" entry))
                    (content
                     (if content-hash
                         (indie-org-webmentions--make-received-wm-content
                          :html (gethash "html" content-hash)
                          :text (gethash "text" content-hash))))
                    (id (gethash "wm-id" entry))
                    (target (gethash "wm-target" entry))
                    (wm
                     (indie-org-webmentions-make-received-wm
                      :id id
                      :sort (indie-org-webmentions--string-to-wm-sort (gethash "wm-property" entry))
                      :time-received
                      (encode-time (parse-time-string (gethash "wm-received" entry)))
                      :source (gethash "wm-source" entry)
                      :target target
                      :author author
                      :content content
                      :private (gethash "wm-private" entry)))
                    (domain-with-authority (concat "https://" domain))
                    ;; throughout this file, "page-key" is described as "any
                    ;; string which uniquely identifies a page"; but _now_ it
                    ;; had better be the page's path on the site. See the
                    ;; comments in indie-org-page-key.el.
                    (page-key (indie-org-page-key-from-url domain-with-authority target))
                    (wms (gethash page-key mentions)))
               ;; `wms' is a list of `indie-org-received-wm'
               ;; Could re-factor to make use of `indie-org-webmentions-add-received-wm'
               (unless
                   (cl-find
                    wm wms
                    :test
                    (lambda (lhs rhs)
                      (eq
                       (indie-org-webmentions-received-wm-id lhs)
                       (indie-org-webmentions-received-wm-id rhs))))
                 (puthash page-key (cons wm wms) mentions)
                 (setq last-id (max id (or last-id 0)))))
             finally
             (message "Processing %d webmentions...done(last-id %d)."
                      num-entries (or last-id 0)))
            (setf (indie-org-webmentions-received-last-id state) last-id)
            (setf (indie-org-webmentions-received-last-checked state) (current-time)))))
    nil))


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

(defun indie-org-webmentions-record-webmention (link info)
  "Note the fact that the current page mentions LINK in INFO.

Update the communications channel property :indie-org/mentions with LINK."
  (let ((mentions (plist-get info :indie-org/mentions)))
    (plist-put info :indie-org/mentions (cons link mentions))))

(defun indie-org-webmentions-browse-mention (path _)
  "Follow a mention link PATH."
  (browse-url path))

(defun indie-org-webmentions-export-mention (link description backend info)
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
    (indie-org-webmentions-record-webmention link info)
    (format "<a href=\"%s\">%s</a>" link description)))

(defun indie-org-webmentions-export-reply (link description backend info)
  "Export a reply webmention LINK/DESCRIPTION for backend BACKEND with INFO."
  (when (org-export-derived-backend-p backend 'html)
    (indie-org-webmentions-record-webmention link info)
    (format "<a href=\"%s\" class=\"u-in-reply-to\">%s</a>" link description)))

(defun indie-org-webmentions-export-like (link description backend info)
  "Export a like webmention LINK/DESCRIPTION for backend BACKEND with INFO."
  (when (org-export-derived-backend-p backend 'html)
    (indie-org-webmentions-record-webmention link info)
    (format "<a href=\"%s\" class=\"u-like-of\">%s</a>" link description)))

(defun indie-org-webmentions-export-repost (link description backend info)
  "Export a repost webmention LINK/DESCRIPTION for backend BACKEND with INFO."
  (when (org-export-derived-backend-p backend 'html)
    (indie-org-webmentions-record-webmention link info)
    (format "<a href=\"%s\" class=\"u-repost-of\">%s</a>" link description)))

;;;###autoload
(defun indie-org-webmentions-enable ()
  "Enable webmentions support."
  (org-link-set-parameters
   "mention"
   :follow #'indie-org-webmentions-browse-mention
   :export #'indie-org-webmentions-export-mention)
  (org-link-set-parameters
   "reply"
   :follow #'indie-org-webmentions-browse-mention
   :export #'indie-org-webmentions-export-reply)
  (org-link-set-parameters
   "like"
   :follow #'indie-org-webmentions-browse-mention
   :export #'indie-org-webmentions-export-like)
  (org-link-set-parameters
   "repost"
   :follow #'indie-org-webmentions-browse-mention
   :export #'indie-org-webmentions-export-repost))

(provide 'indie-org-webmentions)
;;; indie-org-webmentions.el ends here.
