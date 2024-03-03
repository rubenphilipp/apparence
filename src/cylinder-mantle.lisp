;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/cylinder-mantle
;;; NAME
;;; cylinder-mantle
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-03
;;; 
;;; PURPOSE
;;; This class implements the cylinder mantle projection surface. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> canvas -> projection-surface -> cylinder-mantle
;;;
;;; $$ Last modified:  18:40:34 Sun Mar  3 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass cylinder-mantle (projection-surface)
  ;; NB: diameter and width should never be assigned at the same time
  ;; NB2: the width in this case is the circumference of cylinder
  ((surface-diameter :accessor surface-diameter :initarg :surface-diameter
                     :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((cm cylinder-mantle) &rest initargs)
  (declare (ignore initargs))
  (update cm))

(defmethod print-object :before ((cm cylinder-mantle) stream)
  (format stream "~%CYLINDER-MANTLE: surface-diameter: ~a"
          (surface-diameter cm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((cm cylinder-mantle))
  (clone-with-new-class cm 'cylinder-mantle))

(defmethod clone-with-new-class :around ((cm cylinder-mantle) new-class)
  (declare (ignore new-class))
  (let ((new (call-next-method)))
    (setf (slot-value new 'surface-diameter) (surface-diameter cm))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defmethod (setf surface-diameter) :before (value (cm cylinder-mantle))
  (unless (> value 0)
    (error "cylinder-mantle::(setf surface-diameter): The surface-diameter ~
            must be > 0"))
  (set-width cm)
  (update cm))

(defmethod (setf width) :before (value (cm cylinder-mantle))
  (unless (> value 0)
    (error "cylinder-mantle::(setf width): The width must be > 0"))
  (set-diameter cm)
  (update cm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-diameter ((cm cylinder-mantle))
  (let ((width (surface-width cm)))
    (setf (slot-value cm 'surface-diameter)
          (/ width pi))))

(defmethod set-width ((cm cylinder-mantle) &rest ignore)
  (declare (ignore ignore))
  ;; NB: this is, in this case, the circumference
  (let ((diameter (surface-diameter cm)))
    (setf (slot-value cm 'surface-width)
          (* diameter pi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update :before ((cm cylinder-mantle) &key ignore)
  (declare (ignore ignore))
  (unless (initialized cm)
    (if (and (surface-width cm) (surface-diameter cm))
        (error "cylinder-mantle::initialize-instance: You can't ~
                specify both the surface-diameter and the surface-width ~
                slots. ~a" cm))
    (let ((diameter (surface-diameter cm))
          (width (surface-width cm)))
      (cond ((and diameter (not width))
             (set-width cm))
            ((and width (not diameter))
             (set-diameter cm))
            ((and width diameter)
             (error "cylinder-mantle::initialize-instance: You can't ~
                   specify both the surface-diameter and the surface-width ~
                   slots. ~a" cm))))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cylinder-mantle.lisp
