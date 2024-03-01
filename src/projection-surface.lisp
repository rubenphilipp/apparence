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
;;; scale from those of the actual canvas. The canvas-coordinates are derived
;;; via an x- and y-scaler or a relative value when transferring/rendering a ps
;;; to a canvas.  The coordinate values of projection-surfaces are -- other than
;;; those of a canvas -- not limited to integer values, but can also be
;;; e.g. floats.
;;;
;;; The data slot of a projection-surface holds a list of projection objects
;;; which will be successively parsed when transferring/rendering a ps to a
;;; canvas.
;;;
;;; CLASS HIERARCHY
;;; named-object -> projection-surface
;;;
;;; $$ Last modified:  18:33:53 Fri Mar  1 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass projection-surface (named-object)
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((ps projection-surface) &rest initargs)
  (declare (ignore initargs))
  (update ps))

(defmethod print-object :before ((ps projection-surface) stream)
  (format stream "~%PROJECTION-SURFACE: width: ~a, height: ~a"
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
;;; A projection surface is an abstraction of a canvas. Its purpose is to be
;;; enable the (spatial) arrangement of visual information (images etc.)
;;; independently from the actual pixel-dimensions of the canvas. Thus, the
;;; dimensions of and the coordinates on the projection-surface can differ in
;;; scale from those of the actual canvas. The canvas-coordinates are derived
;;; via an x- and y-scaler or a relative value when transferring/rendering a ps
;;; to a canvas.  The coordinate values of projection-surfaces are -- other than
;;; those of a canvas -- not limited to integer values, but can also be
;;; e.g. floats.
;;;
;;; The data slot of a projection-surface holds a list of projection objects
;;; which will be successively parsed when transferring/rendering a ps to a
;;; canvas.
;;;
;;; ARGUMENTS
;;; - The width of the projection surface.
;;; - The height of the projection surface.
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
