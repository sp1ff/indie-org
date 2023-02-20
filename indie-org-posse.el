;;; indie-org-posse.el -- POSSE support    -*- lexical-binding: t -*-

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

;; This package provides POSSE support for the indie-org package.  POSSE stands
;; for Post on Own Site, Syndicate Elsewhere (or Everywhere).

;;; Code:
(require 'request)

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Utility Code                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-posse-string-to-target (text)
  "Convert a #+POSSE value TEXT to keyword."
  (cond
   ((string= text "flickr"  ) :flickr)
   ((string= text "github"  ) :github)
   ((string= text "mastodon") :mastodon)
   ((string= text "reddit"  ) :reddit)
   ((string= text "twitter" ) :twitter)
   (t (error "Unknown POSSE target ``%s''" text))))

(defun indie-org-posse-target-to-string (target)
  "Convert a POSSE TARGET keyword to a human-friendly string."
  (cond
    ((eq target :flickr  ) "flickr"  )
    ((eq target :github  ) "github"  )
    ((eq target :mastodon) "mastodon")
    ((eq target :reddit  ) "reddit"  )
    ((eq target :twitter ) "twitter" )
    (t (error "Unknown POSSE target %s" target))))

(defun indie-org-posse-target-to-wm-target (target)
  "Convert a POSSE keyword to the corresponding webmention target at brid.gy.
TARGET shall be a keyword (:twitter, e.g.)"
  (concat
   "https://brid.gy/publish/"
   (cond
    ((eq target :flickr  ) "flickr"  )
    ((eq target :github  ) "github"  )
    ((eq target :mastodon) "mastodon")
    ((eq target :reddit  ) "reddit"  )
    ((eq target :twitter ) "twitter" )
    (t (error "Unknown POSSE target %s" target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              POSSE requests                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (indie-org-posse-requests
               (:constructor nil)
               (:constructor indie-org-posse-make-requests))
  "Mapping from page-key to POSSE targets.
Targets are represented as lists of target keywords (:twitter, :mastodon, &c)."
  ;; 👇 is a hash table mapping page-key to list of posse requests
  (hash (make-hash-table :test 'equal) :type hash-table))

(defun indie-org-posse-pp-requests (req &optional indent)
  "Pretty-print REQ at indentation level INDENT."
  (unless (indie-org-posse-requests-p req)
    (signal 'wrong-type-argument (list #'indie-org-posse-requests-p req)))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s))
         (indent2 (make-string (+ 4 (* indent 4)) ?\s))
         (hash (indie-org-posse-requests-hash req))
         (page-keys (indie-org-webmentions--sorted-hash-string-keys hash)))
    (message "%sPOSSE requests by page:" indent1)
    (maphash
     ;;      `posse-reqs' 👇 is a list of POSSE target keywords
     (lambda (page-key posse-reqs)
       (if posse-reqs
           (message "%s%s: %s" indent2 page-key
            (mapconcat #'indie-org-posse-target-to-string posse-reqs " "))))
     hash))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              POSSE responses                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (indie-org-posse-responses
               (:constructor nil)
               (:constructor indie-org-posse-make-responses))
  "Mapping from page-key to POSSE responses."
  ;; 👇 is a hash table mapping page-key to lists of `indie-org-posse-response' instances.
  (hash (make-hash-table :test 'equal)))

(defun indie-org-posse-pp-responses (responses &optional indent)
    "Pretty-print RESPONSES instance at indentation level INDENT."
  (unless (indie-org-posse-responses-p responses)
    (signal 'wrong-type-argument (list #'indie-org-posse-responses-p responses )))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s))
         (indent2 (make-string (+ 4 (* indent 4)) ?\s))
         (hash (indie-org-posse-responses-hash responses)))
    (message "%sPOSSE responses by page:" indent1)
    (maphash
     (lambda (page-key rsps)
       (when rsps
           (message "%s%s:" indent2 page-key)
           (while rsps
             (indie-org-posse-pp-response (car rsps) (+ 2 indent))
             (setq rsps (cdr rsps)))))
     hash)))

(defun indie-org-posse-responses-for-page-key (responses page-key)
    "Return the POSSE responses we've received for PAGE-KEY.
RESPONSES is an `indie-org-posse-responses' instance."
  (unless (indie-org-posse-responses-p responses)
    (signal 'wrong-type-argument (list #'indie-org-posse-responses-p responses)))
  (gethash page-key (indie-org-posse-responses-hash responses)))

(cl-defstruct (indie-org-posse-response
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                indie-org-posse-make-response
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

(defun indie-org-posse-pp-response (rsp &optional indent)
    "Pretty-print RSP at indentation level INDENT."
  (unless (indie-org-posse-response-p rsp)
    (signal 'wrong-type-argument (list #'indie-org-posse-response-p rsp)))
  (let* ((indent (or indent 0))
         (indent1 (make-string (* indent 4) ?\s)))
    (message "%s        ID: %s\n%sCreated At: %s\n%s      Sort: %s\n%s      Text: %S\n%s       URL: %s"
             indent1 (indie-org-posse-response-id rsp)
             indent1 (indie-org-posse-response-created-at rsp)
             indent1 (indie-org-posse-response-sort rsp)
                      indent1 (indie-org-posse-response-text rsp)
                      indent1 (indie-org-posse-response-url rsp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             syndication links                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (indie-org-posse-syndicated-copies
               (:constructor nil)
               (:constructor indie-org-posse-make-syndicated-copies))
  "Mapping from page-key to syndicated copies."
  ;; 👇 is a hash table mapping page key to lists of `indie-org-posse-syndicated-copy'
  (hash (make-hash-table :test 'equal) :type hash-table))

(defun indie-org-posse-pp-syndicated-copies (copies &optional indent)
    "Pretty-print COPIES at indentation level INDENT."
    (unless (indie-org-posse-syndicated-copies-p copies)
      (signal 'wrong-type-argument (list #'indie-org-posse-syndicated-copies-p copies)))
    (let* ((indent (or indent 0))
           (indent1 (make-string (* indent 4) ?\s))
           (hash (indie-org-posse-syndicated-copies-hash copies)))
      (maphash
       (lambda (page-key copies)
         (if copies
             (while copies
               (indie-org-posse-pp-syndicated-copy (car copies) (1+ indent))
               (setq copies (cdr copies)))))
       hash)))

(cl-defstruct (indie-org-posse-syndicated-copy
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                indie-org-posse-make-syndicated-copy
                (&key silo url type id
                      &aux
                      (_
                       (unless (memq silo '(:twitter :mastodon))
                         (error "%s not supported" silo))))))
  "A syndicated copy of a post"
  silo url type id)

(defun indie-org-posse-pp-syndicated-copy (copy &optional indent)
    "Pretty-print COPY at indentation level INDENT."
  (unless (indie-org-posse-syndicated-copy-p copy)
    (signal 'wrong-type-argument (list #'indie-org-posse-syndicated-copy-p copy)))
  (let* (((indent (or indent 0))
          (indent1 (make-string (* indent 4) ?\s))))
    (message "%s  ID: %s\n%sSilo: %s\n%s URL: %s\n%sType: %s"
             indent1 (indie-org-posse-syndicated-copy-id copy)
             indent1 (indie-org-posse-syndicated-copy-silo copy)
             indent1 (indie-org-posse-syndicated-copy-url url)
             indent1 (indie-org-posse-syndicated-copy-type type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              public methods                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indie-org-send-posse-request (source posse-target)
  "Send a POSSE request to brid.gy.
SOURCE is the page to be published (i.e. its full URL).
POSSE-TARGET is one of :twitter or :mastodon.

Note that SOURCE will need to have a (possibly empty) link to the
brid.gy webmention endpoint corresponding to target on the page
somewhere for the webmention we're about to send to be
accepted (<a href=\"https://brid.gy/publish/twitter\"></a>, e.g.).

Return an indie-org-posse-response."
  (let ((target (indie-org-posse-target-to-wm-target posse-target))
        (rsp))
    (message "Sending POSSE request %S :=> %S" source target)
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
                           (indie-org-posse-make-response
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
                           (setq rsp (indie-org-posse-make-response
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

(defun indie-org-record-posse-request (posse-targets page-key posse-requests)
  "Record one or more POSSE targets.
POSSE-TARGETS is a space-delimited list of POSSE, or a list of their
corresponding symbols (:twitter & so forth).
targets (\"twitter\", \"mastodon\", and so forth).  PAGE-KEY is the key
naming the page to be POSSE'd.  POSSE-REQUESTS is an
`indie-org-posse-requests' instance containing a hash table
mapping page-key to POSSE requests."
  (unless (indie-org-posse-requests-p posse-requests)
    (signal 'wrong-type-argument (list 'indie-org-posse-requests-p posse-requests)))
  (puthash
    page-key
    (if (stringp posse-targets)
        (mapcar #'indie-org-posse-string-to-target (split-string posse-targets))
      posse-targets)
    (indie-org-posse-requests-hash posse-requests))
  nil)

(defun indie-org-record-sent-posse (page-key response posse-responses)
  "Record a RESPONSE to PAGE-KEY.
POSSE-RESPONSES shall be an `indie-org-posse-responses' instance."
  (unless (indie-org-posse-responses-p posse-responses)
    (signal 'wrong-type-argument (list 'indie-org-posse-responses-p posse-responses)))
  (let ((hash (indie-org-posse-responses-hash posse-responses)))
    (puthash
     page-key
     (cons response (gethash page-key hash))
     hash)))

(defun indie-org-posse-required (requests responses)
  "Determine the set of POSSE requests to be made.
REQUESTS shall be an `indie-org-posse-requests' instance
describing the POSSE requests for the site.  RESPONSES shall be
collection of previously completed POSSE requests for the current
publication environment, in the form of an
`indie-org-posse-responses' instance.

Return a list of cons cells, each of whose car is a page key
and whose cdr is a list of POSSE symbols."
  (let ((results)
        (responses (indie-org-posse-responses-hash responses)))
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
     (indie-org-posse-requests-hash requests))
    results))


(provide 'indie-org-posse)
;;; indie-org-posse.el ends here.