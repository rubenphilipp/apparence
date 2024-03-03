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
;;; The scalers determine the ratio canvas / projection-surface
;;; (e.g. canvas-width / surface-width).
;;;
;;; NB: Altering the surface-width and -height after initialization will resize
;;;     the canvas applying the given scaling-factors. Changing the scaling
;;;     factors, though, will not change the dimensions of the canvas but those
;;;     of the projection-surface. Finally, changing the canvas dimensions will
;;;     change the surface-width and/or -height.
;;;
;;; The coordinate values of projection-surfaces are -- other than those of a
;;; canvas -- not limited to integer values, but can also be e.g. floats.
;;;
;;; CLASS HIERARCHY
;;; named-object -> canvas -> projection-surface
;;;
;;; $$ Last modified:  22:27:50 Sun Mar  3 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass projection-surface (canvas)
  (;; the surface-width and -height are related to the canvas dimensions
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
    (setf (slot-value new 'initialized) (initialized ps))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf width) :around (value (ps projection-surface))
  (when (initialized ps)
    (setf (slot-value ps 'surface-width) (* value (x-scaler ps))))
    ;;(setf (slot-value ps 'x-scaler) (/ (surface-width ps) value)))
  (call-next-method))

(defmethod (setf height) :around (value (ps projection-surface))
  (when (initialized ps)
    (setf (slot-value ps 'surface-height) (* value (y-scaler ps))))
    ;;(setf (slot-value ps 'y-scaler) (/ (surface-height ps) value)))
  (call-next-method))

(defmethod (setf surface-width) :after (value (ps projection-surface))
  (unless (> (surface-width ps) 0)
    (error "projection-surface::(setf surface-width): The surface-width ~
            must be > 0"))
  (when (initialized ps)
    (setf (width ps) (floor (/ value (x-scaler ps))))))

(defmethod (setf surface-height) :after (value (ps projection-surface))
  (unless (> (surface-height ps) 0)
    (error "projection-surface::(setf surface-height): The surface-height ~
            must be > 0"))
  (when (initialized ps)
    (setf (height ps) (floor (/ value (y-scaler ps))))))

(defmethod (setf x-scaler) :after (value (ps projection-surface))
  (when (initialized ps)
    (setf (slot-value ps 'surface-width) (* value (width ps)))))
    ;;(setf (width ps) (round (* value (width ps))))))

(defmethod (setf y-scaler) :after (value (ps projection-surface))
  (when (initialized ps)
    (setf (slot-value ps 'surface-height) (* value (height ps)))))
    ;;(setf (height ps) (round (* value (height ps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod init-x-dimensions ((ps projection-surface))
  (cond ((and (x-scaler ps) (surface-width ps))
         (setf (slot-value ps 'width) (round (/ (surface-width ps)
                                                (x-scaler ps))))
         t)
        ((and (not (x-scaler ps)) (width ps) (surface-width ps))
         (setf (slot-value ps 'x-scaler) (/ (surface-width ps)
                                            (width ps)))
         t)
        ((and (x-scaler ps) (width ps) (not (surface-width ps)))
         (setf (slot-value ps 'surface-width) (* (x-scaler ps)
                                                 (width ps))))
        ((and (x-scaler ps) (surface-width ps) (width ps))
         (print (width ps))
         (let ((test-res (* (width ps) (x-scaler ps))))
           (when (/= test-res (surface-width ps))
             (error "projection-surface::init-x-dimensions: Inconsistent data: ~
                       width * x-scaler != surface-width. ~% ~
                       (~a * ~a != ~a; surface-width should be ~a)."
                    (width ps) (x-scaler ps) (surface-width ps)
                    test-res)))
         t)
        (t nil)))

(defmethod init-y-dimensions ((ps projection-surface))
  (cond ((and (y-scaler ps) (surface-height ps))
         (setf (slot-value ps 'height) (round (/ (surface-height ps)
                                                 (y-scaler ps)))))
        ((and (not (y-scaler ps)) (height ps) (surface-height ps))
         (setf (slot-value ps 'y-scaler) (/ (surface-height ps)
                                            (height ps))))
        ((and (y-scaler ps) (height ps) (not (surface-height ps)))
         (setf (slot-value ps 'surface-height) (* (y-scaler ps)
                                                  (height ps))))
        ((and (y-scaler ps) (surface-height ps) (height ps))
         (let ((test-res (* (height ps) (y-scaler ps))))
           (when (/= test-res (surface-height ps))
             (error "projection-surface::init-y-dimensions: Inconsistent data: ~
                       height * y-scaler != surface-height. ~% ~
                       (~a * ~a != ~a; surface-height should be ~a)."
                    (height ps) (y-scaler ps) (surface-height ps)
                    test-res))))
        ;;; try to derive from x-dimensions
        ((numberp (x-scaler ps))
         (setf (slot-value ps 'y-scaler) (x-scaler ps)
               (slot-value ps 'surface-height) (* (y-scaler ps)
                                                  (surface-width ps))))
        (t (error "projection-surface::update: Neither an y-scaler nor a ~
                     surface-height are given."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update :before ((ps projection-surface) &key ignore)
  (declare (ignore ignore))
  ;; initialize
  (when (and (not (initialized ps))
             (or (width ps) (height ps) (surface-width ps) (surface-height ps)
                 (x-scaler ps) (y-scaler ps)))
    (cond ((init-x-dimensions ps)
           ;; just go ahead
           (init-y-dimensions ps))
          ((or (y-scaler ps) (surface-height ps) (height ps))
           ;; try to derive the x-dimensions from the y-scaler,
           ;; assuming to keep aspect ratio of image
           (init-y-dimensions ps)
           (setf (slot-value ps 'x-scaler) (y-scaler ps))
           ;; try it again
           (init-x-dimensions ps))
          (t (error "projection-surface::update: Neither an x-scaler nor a ~
                     surface-width nor y-dimensions are given.")))))

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
;;; The scalers determine the ratio canvas / projection-surface
;;; (e.g. canvas-width / surface-width).
;;;
;;; NB1: Altering the surface-width and -height after initialization will resize
;;;      the canvas applying the given scaling-factors. Changing the scaling
;;;      factors, though, will not change the dimensions of the canvas but those
;;;      of the projection-surface. Finally, changing the canvas dimensions will
;;;      change the surface-width and/or -height.
;;;
;;; NB2: Altering the dimensions (surface-width/-height) of the
;;;      projection-surface after initializing might cause the cutting of
;;;      existing content.
;;;
;;; NB3: You need to specify at least a value for :surface-width/-height or
;;;      :x-/y-scaler. If both are specified, it needs to be ensured that width
;;;      * x-scaler = surface-width resp.
;;;      height * y-scaler = surface-height remain true.
;;; 
;;;      It is also possible to just specify one coordinate (x/width or
;;;      y/height). Then, the complementary side will be scaled to the same
;;;      scaler as the side where an argument is given (i.e. this keeps the
;;;      aspect ratio of the canvas), provided :width and :height are given. 
;;;
;;; ARGUMENTS
;;; none. 

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
;;; - :surface-width. The width of the projection surface.
;;; - :surface-height. The height of the projection surface.
;;; - :width. The width (in px, thus an integer) of the canvas. 
;;; - :height. The height (in px, thus an integer) of the canvas. 
;;; - :x-scaler. A scaler (ratio canvas-width/surface-width) to link the
;;;   dimensions of the canvas (in the data slot) to the dimensions of the
;;;   projection.
;;; - :y-scaler. A scaler (ratio canvas-height/surface-height) to link the
;;;   dimensions of the image (in the data slot) to the dimensions of the
;;;   projection.
;;; - :color. The initial color of the canvas, as a rgb(a) list.
;;;   Default = '(0 0 0 0)
;;; - :id. The id of the object. Default = NIL.
;;; - :default-interpolation. The default interpolation method (used e.g. when
;;;   changing the image dimensions via the (setf ...) methods.
;;;   Default = (get-apr-config :default-interpolation)
;;; 
;;; RETURN VALUE
;;; The initialized object. 
;;;
;;; EXAMPLE
#|
(make-projection-surface :surface-width 20
                         :surface-height 40
                         :width 2000
                         :height 4000)
;; =>
PROJECTION-SURFACE: surface-width: 20, surface-height: 40, 
        x-scaler: 1/100, y-scaler: 1/100
CANVAS: width: 2000, height: 4000, color: (0 0 0 0), 
        initialized: T
NAMED-OBJECT: id: NIL, tag: NIL, 
data: 
IMAGE: width: 2000, height: 4000
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<RGB-IMAGE (2000x4000) {700A9A2B03}>
**********
|#
;;; SYNOPSIS
(defun make-projection-surface (&key surface-width
                                  surface-height
                                  x-scaler
                                  y-scaler
                                  width
                                  height
                                  (color '(0 0 0 0))
                                  id)
  ;;; ****
  (make-instance 'projection-surface :surface-width surface-width
                                     :surface-height surface-height
                                     :x-scaler x-scaler
                                     :y-scaler y-scaler
                                     :width width
                                     :height height
                                     :color color
                                     :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; put methods for projections
;;; all values relative to the projection(-surface) scale
;;; assuming coordinates and dimensions of projection-surface and projection
;;; share the same scale!!
;;; RP  Sat Mar  2 21:59:40 2024
(defmethod put-it ((ps projection-surface) (pn projection)
                   &key
                     height
                     width
                     (src-y 0)
                     (src-x 0)
                     (dest-y 0)
                     (dest-x 0)
                     (interpolation
                      (get-apr-config :default-interpolation)))
  ;;; ****
  (unless (initialized ps)
    (error "projection-surface::put-it: The projection-surface object has not ~
            been initialized."))
  (unless (initialized pn)
    (error "projection-surface::put-it: The projection object has not ~
            been initialized."))
  ;;; NB: scalers in ps are different from scalers in pn
  ;;; RP  Sat Mar  2 23:59:24 2024
  (let ((pn->ps-x (/ (x-scaler pn)
                     (x-scaler ps)))
        (pn->ps-y (/ (y-scaler pn)
                   (y-scaler ps)))
         (tmp-pn pn))
    ;; scale the src if necessary
    (unless (= 1.0 pn->ps-x pn->ps-y)
      ;; warn when image is upscaled
      (when (or (< 1.0 pn->ps-x) (< 1.0 pn->ps-y))
        (when (get-apr-config :verbose)
          (warn "projection-surface::put-it: The projection image will be ~
                 upscaled by a factor of ~
                 x: ~a, y: ~a." pn->ps-x pn->ps-y)))
      ;; clone the src
      (setf tmp-pn (make-image (data pn)))
      (scale tmp-pn pn->ps-x pn->ps-y :interpolation interpolation))
    (let ((height (when height (round (/ height (y-scaler pn)))))
          (width (when width (round (/ width (x-scaler pn)))))
          (src-y (round (/ src-y (y-scaler pn))))
          (src-x (round (/ src-x (x-scaler pn))))
          (dest-y (round (/ dest-y (/ 1 (y-scaler ps)))))
          (dest-x (round (/ dest-x (/ 1 (x-scaler ps))))))
      (if (every #'(lambda (x)
                     (or (null x) (< -1 x)))
                 (list height width height src-x src-y))
          (imago::copy (data (data ps)) (data tmp-pn)
                       :height height
                       :width width
                       :src-y src-y
                       :src-x src-x
                       :dest-y dest-y
                       :dest-x dest-x)
          (warn "projection::copy: Won't copy. The resulting dimensions are ~
                 too small.")))
    ps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; put-it-circular for projection-surface objects.
;;; cf. canvas/projection-surface for doc
;;; RP  Sun Mar  3 17:48:35 2024
(defmethod put-it-circular ((ps projection-surface) (pn projection)
                            azimuth y
                            &key
                              height
                              width
                              (src-y 0)
                              (src-x 0)
                              (ps-origin 0.0)
                              (pn-origin 0.5)
                              (interpolation
                               (get-apr-config :default-interpolation))
                              (verbose (get-apr-config :verbose)))
  ;;; ****
  (unless (initialized ps)
    (error "projection-surface::put-it-circular: The projection-surface object ~
            has not been initialized."))
  (unless (initialized pn)
    (error "projection-surface::put-it-circular: The projection object has not ~
            been initialized."))
  (let ((pn->ps-x (/ (x-scaler pn)
                     (x-scaler ps)))
        (pn->ps-y (/ (y-scaler pn)
                   (y-scaler ps)))
        (tmp-pn pn))
    ;; scale the src if necessary
    (unless (= 1.0 pn->ps-x pn->ps-y)
      ;; warn when image is upscaled
      (when (or (< 1.0 pn->ps-x) (< 1.0 pn->ps-y))
        (when (get-apr-config :verbose)
          (warn "projection-surface::put-it-circular: The projection image ~
                 will be upscaled by a factor of ~
                 x: ~a, y: ~a." pn->ps-x pn->ps-y)))
      ;; clone the src
      (setf tmp-pn (make-image (data pn)))
      (scale tmp-pn pn->ps-x pn->ps-y :interpolation interpolation))
    (let ((height (when height (round (/ height (y-scaler pn)))))
          (width (when width (round (/ width (x-scaler pn)))))
          (src-y (round (/ src-y (y-scaler pn))))
          (src-x (round (/ src-x (x-scaler pn))))
          (y (round (/ y (/ 1 (y-scaler ps))))))
      (if (every #'(lambda (x)
                     (or (null x) (< -1 x)))
                 (list height width height src-x src-y))
          (put-it-circular ps tmp-pn azimuth y
                           :verbose verbose
                           :width width
                           :height height
                           :src-x src-x
                           :src-y src-y
                           :image-origin pn-origin
                           :canvas-origin ps-origin)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF projection-surface.lisp
