;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/seq
;;; NAME
;;; seq
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-05
;;; 
;;; PURPOSE
;;; Implementation of the seq class. Objects of this class hold information
;;; on sequential data (e.g. image-file-seqs). The data-slot holds the actual
;;; content, as a an assoc-list (alist) with unique numeric ids. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> seq
;;;
;;; $$ Last modified:  14:55:04 Sat Apr  6 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass seq (named-object)
  ;; no further slots defined
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((sq seq) &rest initargs)
  (declare (ignore initargs))
  (update sq))

(defmethod print-object :before ((sq seq) stream)
  (format stream "~%SEQ: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((sq seq))
  (clone-with-new-class sq 'seq))

(defmethod clone-with-new-class :around ((sq seq) new-class)
  (declare (ignore new-class))
  (let ((new (call-next-method)))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf data) :after (value (sq seq))
  (declare (ignore value))
  (set-data sq))

(defmethod set-data ((sq seq) &rest ignore)
  (declare (ignore ignore))
  (let ((value (data sq)))
    (if (alistp value)
      (if (assoc-unique? value)
          (setf (slot-value sq 'data) value)
          (error "seq::set-value: The ids of the given alist are not unique"))
      (setf (slot-value sq 'data)
            (list->alist value :unique t :sort t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update ((sq seq) &key ignore)
  (declare (ignore ignore))
  (set-data sq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(make-seq '((0001 "/tmp/1.jpg")
            (0002 "/tmp/2.jpg")))
|#
(defun make-seq (data &key id)
  ;;; ****
  (make-instance 'seq :data data
                      :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(let ((seq (make-seq '((1 'one)
                       (2 'two)))))
  (get-item seq 2))

;; => 'TWO
|#
(defmethod get-item ((sq seq) key &key warn?)
  ;;; ****
  (let ((result (assoc-value (data sq) key)))
    (when (and warn? (null result))
      (warn "seq::get-item: There is no data for key \"~a\"." key))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF seq.lisp
