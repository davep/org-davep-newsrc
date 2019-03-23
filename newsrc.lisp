;;; org-davep-newsrc --- Simple ~/.newsrc parser.
;;
;; newsrc.lisp --- Main code for org-davep-newsrc.
;; Copyright 2001-2004 by Dave Pearson <davep@davep.org>
;; $Revision: 1.7 $
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

;;; Code:

(in-package :org.davep.newsrc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global default variables.

(defvar *default-newsrc-file*
  (merge-pathnames (user-homedir-pathname) (make-pathname :name ".newsrc"))
  "Default name of a newsrc file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class for holding group details.

(defclass newsrc-group ()
  ((group-name
    :accessor      group-name
    :type          string
    :documentation "The name of the news group.")
   (subscribed
    :accessor      subscribed
    :type          boolean
    :initform      nil
    :documentation "Is the reader subscribed to this group?")
   (articles
    :accessor      articles
    :type          list
    :initform      nil
    :documentation "List of read article numbers."))
  (:documentation "Class to hold information about a news group.

This class has the following slots:

GROUP-NAME - The name of the news group.
SUBSCRIBED - Are we subscribed to this group?
ARTICLE    - List of read article numbers."))

(defmethod print-object ((group newsrc-group) (stream stream))
  "Format the GROUP for easy reading when output to STREAM."
  (print-unreadable-object (group stream :type t)
    (format stream "~A ~:[un~;~]subscribed" (group-name group) (subscribed group))))

(defmethod fill-from ((group newsrc-group) (line string))
  "Fill GROUP from the text in LINE."
  (let ((group-info (split-sequence-if (lambda (c) (member c '(#\: #\! #\,))) line )))
    (setf (group-name group) (car group-info)
          (articles group)   (loop for articles in (cdr group-info)
                                   for art-list = (split-sequence #\- articles)
                                   collect (if (= (length art-list) 1)
                                               (parse-integer (car art-list))
                                             (cons (parse-integer (car art-list))
                                                   (parse-integer (cadr art-list)))))
          (subscribed group) (char= (char line (length (group-name group))) #\:)))
  group)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class for holding the details of a newsrc file.

(defclass newsrc-file ()
  ((filename
    :accessor      filename
    :initarg       :filename
    :initform      *default-newsrc-file*
    :type          string
    :documentation "Name of the newsrc file.")
   (groups
    :accessor      groups
    :initform      nil
    :type          list
    :documentation "List of groups from the newsrc file"))
  (:documentation "Class to hold the contents of a newsrc file.

This class has the following slots:

FILENAME - The name of the newsrc file.
GROUPS   - List of groups found in the file."))

(defmethod print-object ((newsrc newsrc-file) (stream stream))
  "Format the NEWSRC for easy reading when output to STREAM."
  (print-unreadable-object (newsrc stream :type t)
    (format stream "~S ~S" (filename newsrc) (length (groups newsrc)))))

(defmethod initialize-instance :after ((newsrc newsrc-file) &rest rest)
  "Initialize NEWSRC."
  (declare (ignore rest))
  (with-open-file (rc (filename newsrc) :direction :input)
    (setf (groups newsrc)
          (loop for line = (read-line rc nil nil)
                while line collect (fill-from (make-instance 'newsrc-group) line)))))

;;; newsrc.lisp ends here.
