;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/projection-surface
;;; NAME
;;; projection-surface
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-01
;;; 
;;; PURPOSE
;;; Implementation of the projection-surface class.
;;; 
;;; A projection-surface is an subclass of a canvas. Its purpose is to be enable
;;; the (spatial) arrangement of visual information (images etc.)  independently
;;; from the actual pixel-dimensions of the canvas. Thus, the dimensions of and
;;; the coordinates on the projection-surface can differ in scale from those of
;;; the canvas data itself. 
;;; 
;;; The canvas-coordinates/dimensions are derived by scaling the values
;;; according to a x- and y-scaler. 
;;;
;;; The coordinate values of projection-surfaces are -- other than those of a
;;; canvas -- not limited to integer values, but can also be e.g. floats.
;;;
;;; CLASS HIERARCHY
;;; named-object -> canvas -> projection-surface
;;;
;;; $$ Last modified:  23:50:28 Fri Mar  1 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass projection-surface (canvas)
  (;; the width and height of the ps are related to the canvas dimensions
   ;; through the scalers
   (surface-width :accessor surface-width :initarg :surface-width :initform nil)
   (surface-height :accessor surface-height :initarg :surface-height
                   :initform nil)
   (x-scaler :accessor x-scaler :initarg :x-scaler :initform nil)
   (y-scaler :accessor y-scaler :initarg :y-scaler :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((ps projection-surface) &rest initargs)
  (declare (ignore initargs))
  (update ps))

(defmethod print-object :before ((ps projection-surface) stream)
  (format stream "~%PROJECTION-SURFACE: surface-width: ~a, surface-height: ~a, ~
                  ~%        x-scaler: ~a, y-scaler: ~a"
          (surface-width ps) (surface-height ps) (x-scaler ps) (y-scaler ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((ps projection-surface))
  (clone-with-new-class ps 'projection-surface))

(defmethod clone-with-new-class :around ((ps projection-surface) new-class)
  (declare (ignore new-class))
  (let ((new (call-next-method)))
    (setf (slot-value new 'surface-width) (surface-width ps))
    (setf (slot-value new 'surface-height) (surface-height ps))
    (setf (slot-value new 'x-scaler) (x-scaler ps))
    (setf (slot-value new 'y-scaler) (y-scaler ps))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf surface-width) :after (value (ps projection-surface))
  (declare (ignore value))
  (unless (> (surface-width ps) 0)
    (error "projection-surface::(setf surface-width): The surface-width ~
            must be > 0")))

(defmethod (setf surface-height) :after (value (ps projection-surface))
  (declare (ignore value))
  (unless (> (surface-height ps) 0)
    (error "projection-surface::(setf surface-height): The surface-height ~
            must be > 0")))

(defmethod (setf width) :around (value (ps projection-surface))
  (when (initialized ps)
    (setf (slot-value ps 'x-scaler) (/ value (surface-width ps))))
  (call-next-method))

(defmethod (setf height) :around (value (ps projection-surface))
  (when (initialized ps)
    (setf (slot-value ps 'y-scaler) (/ value (surface-height ps))))
  (call-next-method))

(defmethod (setf surface-width) :after (value (ps projection-surface))
  (when (initialized ps)
    (setf (width ps) (* (floor (* value (x-scaler ps)))))))

(defmethod (setf surface-height) :after (value (ps projection-surface))
  (when (initialized ps)
    (setf (height ps) (* (floor (* value (y-scaler ps)))))))

(defmethod (setf x-scaler) :after (value (ps projection-surface))
  (when (initialized ps)
    (setf (width ps) (floor (* value (surface-width ps))))))

(defmethod (setf y-scaler) :after (value (ps projection-surface))
  (when (initialized ps)
    (setf (height ps) (floor (* value (surface-height ps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update :around ((ps projection-surface) &key ignore)
  (declare (ignore ignore))
  (when (and (surface-width ps) (surface-height ps) (x-scaler ps) (y-scaler ps))
    (let ((c-width (floor (* (x-scaler ps) (surface-width ps))))
          (c-height (floor (* (y-scaler ps) (surface-height ps)))))
      (setf (slot-value ps 'width) c-width
            (slot-value ps 'height) c-height)
      (call-next-method))))

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
;;; A projection-surface is an subclass of a canvas. Its purpose is to be enable
;;; the (spatial) arrangement of visual information (images etc.)  independently
;;; from the actual pixel-dimensions of the canvas. Thus, the dimensions of and
;;; the coordinates on the projection-surface can differ in scale from those of
;;; the canvas data itself. 
;;; 
;;; The canvas-coordinates/dimensions are derived by scaling the values
;;; according to a x- and y-scaler. 
;;;
;;; The coordinate values of projection-surfaces are -- other than those of a
;;; canvas -- not limited to integer values, but can also be e.g. floats.
;;;
;;; NB: Altering the dimensions (surface-width/-height) of the
;;;     projection-surface after initializing might cause the cutting of
;;;     existing content.
;;;
;;; ARGUMENTS
;;; - The width of the projection surface.
;;; - The height of the projection surface.
;;; - The scaling factor for the width of the ps's canvas (number).
;;; - The scaling factor for the height of the ps's canvas (number).
;;; - :color. A three- or four-item list of rgb(a) values determining the canvas
;;;   background color.  Default = '(0 0 0 0)
;;; - :id. The id of the projection-surface. 
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
(make-projection-surface 10 20.5 10.5 10.5)
|#
;;; SYNOPSIS
(defun make-projection-surface (width height x-scaler y-scaler
                                &key id (color '(0 0 0 0)))
  ;;; ****
  (make-instance 'projection-surface :surface-width width
                                     :surface-height height
                                     :x-scaler x-scaler
                                     :y-scaler y-scaler
                                     :color color
                                     :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  put methods for projections

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF projection-surface.lisp
