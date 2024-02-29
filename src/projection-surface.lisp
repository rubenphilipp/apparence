;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/projection-surface
;;; NAME
;;; projection-surface
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-26
;;; 
;;; PURPOSE
;;; This class implements projection projection-surfaces and their positions.
;;; NB: Positions are always in meters. They will be converted later to the
;;; actual pixel values. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> projection-surface
;;;
;;; $$ Last modified:  14:19:55 Thu Feb 29 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass projection-surface (named-object)
  ;; all measures in meters
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((ps projection-surface) &rest initargs)
  (declare (ignore initargs))
  (update ps))

(defmethod print-object :before ((ps projection-surface) stream)
  (format stream "~%PROJECTION-SURFACE: width: ~am, height: ~am"
          (width ps) (height ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update ((ps projection-surface) &key ignore)
  (declare (ignore ignore))
  (unless (> (width ps) 0)
    (error "projection-surface::update: The width must be > 0"))
  (unless (> (height ps) 0)
    (error "projection-surface::update: The height must be > 0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* projection-surface/make-projection-surface
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-26
;;; 
;;; DESCRIPTION
;;; Helper function to instantiate a projection-surface object. 
;;;
;;; ARGUMENTS
;;; - The width of the projection surface (in m).
;;; - The height of the projection surface (in m). 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the object. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; The initialized object. 
;;;
;;; EXAMPLE
#|
(make-projection-surface 10 20.5)
|#
;;; SYNOPSIS
(defun make-projection-surface (width height
                                &key id)
  ;;; ****
  (make-instance 'projection-surface :width width
                                     :height height
                                     :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* projection-surface/derive-canvas-dimensions
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; This method returns the canvas dimensions of a projection-surface object for
;;; rendering purposes. Dimensions can either be calculated by a given scaling
;;; factor (applied to width and height of the projection-surface), or derived
;;; from a scaling-destination (either width or height).
;;;
;;; NB: It is necessary to provide exactly one scaling relation as argument. 
;;;
;;; ARGUMENTS
;;; The projection-surface object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :factor. A scaling factor (number) for width and height.
;;; - :destination-width. A pixel value (number) to scale the dimensions to.
;;; - :destination-height. A pixel value (number) to scale the dimensions to. 
;;; 
;;; RETURN VALUE
;;; A two-item list with width and height of the canvas (in px). 
;;;
;;; EXAMPLE
#|
(let ((ps (make-projection-surface 8.112149 8.373832)))
  (derive-canvas-dimensions ps :destination-width 992))

;; => (992 1024.0)
|#
;;; SYNOPSIS
(defmethod derive-canvas-dimensions ((ps projection-surface)
                                     &key
                                       factor
                                       destination-width
                                       destination-height)
  ;;; ****
  (cond ((and (numberp factor) (not destination-width) (not destination-height))
         (list (* factor (width ps)) (* factor (height ps))))
        ((and (numberp destination-width) (not factor) (not destination-height))
         (let ((factor (/ destination-width (width ps))))
           (list destination-width (* factor (height ps)))))
        ((and (numberp destination-height) (not factor) (not destination-width))
         (let ((factor (/ destination-height (height ps))))
           (list (* factor (width ps)) destination-height)))
        (t (error "projection-surface::derive-canvas-dimensions: Either no ~
                   factor/scaling destination (number) is given or there are ~
                   more than one values present. "))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF projection-surface.lisp
