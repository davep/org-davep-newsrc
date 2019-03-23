;;; org-davep-newsrc --- Simple ~/.newsrc parser.
;;
;; org-davep-newsrc.asd --- asdf package definition file.
;; Copyright 2001-2004 by Dave Pearson <davep@davep.org>
;; $Revision: 1.1 $
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
;;   <URL:http://www.davep.org/lisp/org-davep-newsrc>

(defpackage #:org-davep-newsrc-system
  (:use #:common-lisp #:asdf))

(in-package :org-davep-newsrc-system)

(defsystem org-davep-newsrc
  :name        "org-davep-newsrc"
  :author      "Dave Pearson <davep@davep.org>"
  :maintainer  "Dave Pearson <davep@davep.org>"
  :licence     "LLGPL"
  :version     "2.0"
  :description "Simple ~/.newsrc parser.
org-davep-newsrc provides a classes and functions for Common Lisp that
provide easy access to the content of a Un*x newsrc file.

See <URL:http://www.davep.org/lisp/#org-davep-newsrc> for the latest version
of this package."
  :depends-on  (:split-sequence)
  :components  ((:file "packages")
                (:file "newsrc" :depends-on ("packages"))))

;;; org-davep-newsrc.asd ends here.
