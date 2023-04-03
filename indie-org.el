;;; indie-org.el --- Org HTML Export on the Indieweb -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>
;; Version: 0.5.1
;; Package-Requires: ((emacs "26.1"))
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

;; `indie-org' is an Emacs package for getting statically-generated
;; web sites published with Org-mode's HTML Export facility onto the
;; Indieweb.

;;; Code:
(require 'indie-org-webmentions)
(require 'indie-org-posse)
(require 'indie-org-state)
(require 'indie-org-pub)
(require 'indie-org-h-feed)

(require 'ox)
(require 'ox-rss)

(defconst indie-org-version "0.5.1")

(defgroup indie-org nil
  "Org HTML Export on the Indieweb."
  :group 'org)

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

;;;###autoload
(defun indie-org-enable ()
  "Enable indie-org'.

Define a new export backend (derived from HTML) that will add
DRAFT & UPDATED keywords."
  ;; It would be nice if there was way to "un-define" these backends &
  ;; "un-set" these link parameters so as to enable an
  ;; `indie-org-unload'.
  (indie-org-state-enable)
  (indie-org-pub-enable)
  (indie-org-h-feed-enable)
  (indie-org-webmentions-enable))

(provide 'indie-org)
;;; indie-org.el ends here
