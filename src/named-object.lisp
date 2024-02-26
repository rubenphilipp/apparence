;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/named-object
;;; NAME
;;; named-object
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-26
;;; 
;;; PURPOSE
;;; Implementation of the named-object class which is the base class for
;;; all apparence classes.
;;;
;;; CLASS HIERARCHY
;;; None, as this is the base class for all apparence classes.
;;;
;;; $$ Last modified:  15:33:16 Mon Feb 26 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass named-object ()
  ((id :accessor id :initarg :id :initform nil)
   ;; just for identification, not searching purposes
   (tag :accessor tag :initarg :tag :initform nil)
   (data :accessor data :initarg :data :initform nil)))


(defmethod initialize-instance :after ((no named-object) &rest initargs)
  (declare (ignore initargs))
  (check-named-object-id-type (id no)))


(defmethod print-object :after ((no named-object) stream)
  (format stream "~&**********~%"))


(defmethod print-object ((no named-object) stream)
  (let* ((id (id no))
         (data (data no))
         (id-print (if (stringp id)
                       (concatenate 'string "\"" id "\"")
                       id)))
    (format stream "~%NAMED-OBJECT: id: ~a, tag: ~a, ~&data: ~a"
            id-print
            (tag no)
            data)))


(defmethod (setf id) :before (value (no named-object))
  (check-named-object-id-type value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Validate the id of the named-object. NB: NIL is a valid id.
;;; 2023-07-15

(defun check-named-object-id-type (id)
  (when id
    (unless (or (stringp id)
                (symbolp id)
                (numberp id))
      (error "named-object::check-named-object-id-type: ~
              The id slot of the named-object ~%(or it's subclasses) must ~
              be a string, a symbol or a number. Your id is ~%~a" id)))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF named-object.lisp
