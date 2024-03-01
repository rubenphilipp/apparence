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
;;; A projection-surface is an abstraction of a canvas. Its purpose is to be
;;; enable the (spatial) arrangement of visual information (images etc.)
;;; independently from the actual pixel-dimensions of the canvas. Thus, the
;;; dimensions of and the coordinates on the projection-surface can differ in
;;; scale from those of the actual canvas.
;;; 
;;; The canvas-coordinates/dimensions are derived by scaling the values
;;; according to a x- and y-scaler. The canvas itself resides in the data-slot
;;; of the projection-surface.
;;;
;;; The coordinate values of projection-surfaces are -- other than those of a
;;; canvas -- not limited to integer values, but can also be e.g. floats.
;;;
;;; CLASS HIERARCHY
;;; named-object -> projection-surface
;;;
;;; $$ Last modified:  21:54:23 Fri Mar  1 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass projection-surface (named-object)
  (;; the width and height of the ps (not the canvas)
   (width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (canvas-x-scaler :accessor canvas-x-scaler :initarg :canvas-x-scaler
                    :initform nil)
   (canvas-y-scaler :accessor canvas-y-scaler :initarg :canvas-y-scaler
                    :initform nil)
   (canvas-color :accessor canvas-color :initarg :canvas-color
                 :initform '(0 0 0 0))
   (initialized :accessor initialized :initform nil)))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((ps projection-surface) &rest initargs)
  (declare (ignore initargs))
  (update ps))

(defmethod print-object :before ((ps projection-surface) stream)
  (format stream "~%PROJECTION-SURFACE: width: ~a, height: ~a, ~
                  ~%        canvas-x-scaler: ~a, canvas-y-scaler: ~a, ~
                  ~%        canvas-color: ~a, initialized: ~a"
          (width ps) (height ps) (canvas-x-scaler ps) (canvas-y-scaler ps)
          (color->list (canvas-color ps)) (initialized ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((ps projection-surface))
  (clone-with-new-class ps 'projection-surface))

(defmethod clone-with-new-class :around ((ps projection-surface) new-class)
  (declare (ignore new-class))
  (let ((new (call-next-method)))
    (setf (slot-value new 'width) (width ps))
    (setf (slot-value new 'height) (height ps))
    (setf (slot-value new 'canvas-x-scaler) (canvas-x-scaler ps))
    (setf (slot-value new 'canvas-y-scaler) (canvas-y-scaler ps))
    (setf (slot-value new 'canvas-color) (canvas-color ps))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update ((ps projection-surface) &key ignore)
  (declare (ignore ignore))
  (unless (typep (data ps) 'canvas)
    ;;; initialize
    (when (and (width ps) (height ps) (canvas-x-scaler ps) (canvas-y-scaler ps)
               (canvas-color ps))
      (set-color ps)
      (let ((canvas-width (floor (* (width ps) (canvas-x-scaler ps))))
            (canvas-height (floor (* (height ps) (canvas-y-scaler ps)))))
        (setf (slot-value ps 'data) (make-canvas canvas-width canvas-height
                                                 :color (canvas-color ps))
              (slot-value ps 'initialized) t)))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf height) :after (value (ps projection-surface))
  (declare (ignore value))
  (unless (> (height ps) 0)
    (error "projection-surface::(setf height): The height must be > 0")))

(defmethod (setf width) :after (value (ps projection-surface))
  (declare (ignore value))
  (unless (> (width ps) 0)
    (error "projection-surface::(setf width): The width must be > 0")))

(defmethod (setf color) :after (value (ps projection-surface))
  (declare (ignore value))
  (when (data ps)
    (error "projection-surface::(setf color): You can't change the color after ~
            instantiation as this would delete the content of the ~
            projection-surface."))
  (set-color ps))

(defmethod set-color ((ps projection-surface) &rest ignore)
  (declare (ignore ignore))
  (let ((c (canvas-color ps)))
    ;; just convert color list when a list is given
    (when (or (rgb-p c) (rgba-p c))
      (setf (slot-value ps 'canvas-color) (apply #'make-color c)))))

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
;;; A projection-surface is an abstraction of a canvas. Its purpose is to be
;;; enable the (spatial) arrangement of visual information (images etc.)
;;; independently from the actual pixel-dimensions of the canvas. Thus, the
;;; dimensions of and the coordinates on the projection-surface can differ in
;;; scale from those of the actual canvas.
;;; 
;;; The canvas-coordinates/dimensions are derived by scaling the values
;;; according to a x- and y-scaler. The canvas itself resides in the data-slot
;;; of the projection-surface.
;;;
;;; The coordinate values of projection-surfaces are -- other than those of a
;;; canvas -- not limited to integer values, but can also be e.g. floats.
;;;
;;; NB: Altering the dimensions (width/height) of the projection-surface after
;;;     initializing might cause the cutting of existing content. 
;;;
;;; ARGUMENTS
;;; - The width of the projection surface.
;;; - The height of the projection surface.
;;; - The scaling factor for the width of the ps's canvas (number).
;;; - The scaling factor for the height of the ps's canvas (number).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the object. Default = NIL.
;;; - :canvas-color. The default color of the canvas as a rgb(a) color list.
;;;   Default = '(0 0 0 0)
;;; 
;;; RETURN VALUE
;;; The initialized object. 
;;;
;;; EXAMPLE
#|
(make-projection-surface 10 20.5 10.5 10.5)
|#
;;; SYNOPSIS
(defun make-projection-surface (width height canvas-x-scaler canvas-y-scaler
                                &key id (canvas-color '(0 0 0 0)))
  ;;; ****
  (make-instance 'projection-surface :width width
                                     :height height
                                     :canvas-x-scaler canvas-x-scaler
                                     :canvas-y-scaler canvas-y-scaler
                                     :canvas-color canvas-color
                                     :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* projection-surface/make-canvas-from-ps
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-29
;;; 
;;; DESCRIPTION
;;; This method creates a canvas object from a projection-surface. The
;;; dimensions of the canvas will be derived from the dimensions of the ps. This
;;; is done by internally applying derive-canvas-dimensions. Hence, it is
;;; mandatory to give exactly one of the three keyword-arguments (either
;;; :factor, :destination-width, or :destination-height) in order to get a
;;; properly scaled canvas. 
;;;
;;; ARGUMENTS
;;; - A projection-surface object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;;   Give exactly one of these three arguments, leave the other to at NIL.
;;; - :factor. A scaling factor (number) for width and height.
;;; - :destination-width. A pixel value (number) to scale the dimensions to.
;;; - :destination-height. A pixel value (number) to scale the dimensions to.
;;; 
;;; - :color. A three- or four-item list of rgb(a) values determining the canvas
;;;   background color.  Default = '(0 0 0 0)
;;; - :id. The id of the canvas. 
;;; 
;;; RETURN VALUE
;;; The canvas object. 
;;;
;;; EXAMPLE
#|
(let ((ps (make-projection-surface 200 300)))
  (make-canvas-from-ps ps :factor 2.5))

;; =>
CANVAS: width: 500, height: 750, color: (0 0 0 0)
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<RGB-IMAGE (500x750) {7015211513}>
|#
;;; SYNOPSIS
(defmethod make-canvas-from-ps ((ps projection-surface)
                                &key
                                  ;; just one of these three
                                  factor destination-width destination-height
                                  ;; any of these can be set
                                  (color '(0 0 0 0))
                                  id)
  ;;; ****
  (let ((cv-dimensions (mapcar #'floor
                               (derive-canvas-dimensions
                                ps :factor factor
                                   :destination-width destination-width
                                   :destination-height destination-height))))
    (make-canvas (first cv-dimensions) (second cv-dimensions)
                 :color color :id id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF projection-surface.lisp
