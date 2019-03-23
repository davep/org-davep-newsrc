;;; org-davep-newsrc --- Simple ~/.newsrc parser.
;;
;; packages.lisp --- Defines packages for org-davep-newsrc.
;; Copyright 2001-2004 by Dave Pearson <davep@davep.org>
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2001-2004.
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

;;; Commentary:
;;
;; The following code provides a couple of classes that let you easily read
;; the content of a newsrc file.
;;
;; You can always find the latest version of this code at:
;;
;;   <URL:https://github.com/davep/org-davep-newsrc>

;; Create the newsrc package.
(defpackage #:org.davep.newsrc
  (:nicknames #:newsrc)
  (:use #:common-lisp #:split-sequence)
  #-(or allegro cormanlisp) (:documentation "~/.newsrc parsing tools.")
  (:export "*DEFAULT-NEWSRC-FILE*"      ; Global config variable.
           "NEWSRC-GROUP"               ; NEWSRC-GROUP.
           "GROUP-NAME"
           "SUBSCRIBED"
           "ARTICLES"
           "NEWSRC-FILE"                ; NEWSRC-FILE.
           "FILENAME"
           "GROUPS"))

;;; packages.lisp ends here.
