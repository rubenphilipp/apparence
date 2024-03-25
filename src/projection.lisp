;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/projection
;;; NAME
;;; projection
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-02
;;; 
;;; PURPOSE
;;; Implementation of the projection class.
;;;
;;; A projection is a subclass of an image. Its purpose is to "translate" the
;;; dimensions of an image object to the dimensions of the projection surface.
;;; The data-slot holds the actual (unscaled) image object with its original
;;; dimensions and properties. Its relation to the projection-surface is
;;; determined by the projection-width and -height slots as well as by the
;;; related x- and y-scalers.
;;;
;;; The scalers determine the ratio projection / image (e.g. projection-width /
;;; image-width).
;;; 
;;; NB: Altering the projection-width and -height after initialization will
;;;     resize the image applying the given scaling-factors. Changing the
;;;     scaling factors, though, will not change the dimensions of the image
;;;     but those of the projection. Finally, changing the image dimensions will
;;;     change the projection-width and/or -height. 
;;;
;;; The coordinate values of projections are -- other than those of images --
;;; not limited to integer values, but can also be e.g. floats.
;;;
;;; CLASS HIERARCHY
;;; named-object -> image -> projection
;;;
;;; $$ Last modified:  16:43:39 Mon Mar 25 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass projection (image)
  (;; the projection-width and -height are related to the image dimensions
   ;; through the scalers
   (projection-width :accessor projection-width :initarg :projection-width
                     :initform nil)
   (projection-height :accessor projection-height :initarg :projection-height
                      :initform nil)
   (x-scaler :accessor x-scaler :initarg :x-scaler :initform nil)
   (y-scaler :accessor y-scaler :initarg :y-scaler :initform nil)
   (initialized :accessor initialized :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((pn projection) &rest initargs)
  (declare (ignore initargs))
  (update pn))


(defmethod print-object :before ((pn projection) stream)
  (format stream "PROJECTION: projection-width: ~a, projection-height: ~a ~
                  ~%        x-scaler: ~a, y-scaler: ~a, initialized: ~a"
          (projection-width pn) (projection-height pn)
          (x-scaler pn) (y-scaler pn) (initialized pn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((pn projection))
  (clone-with-new-class pn 'projection))

(defmethod clone-with-new-class :around ((pn projection) new-class)
  (declare (ignore new-class))
  (let ((new (call-next-method)))
    (setf (slot-value new 'projection-width) (projection-width pn))
    (setf (slot-value new 'projection-height) (projection-height pn))
    (setf (slot-value new 'x-scaler) (x-scaler pn))
    (setf (slot-value new 'y-scaler) (y-scaler pn))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf width) :after (value (pn projection))
  ;; changing the width alters the projection-width
  (when (initialized pn)
    (setf (slot-value pn 'projection-width) (* value (x-scaler pn)))))

;; apropos :before see (setf width)
(defmethod (setf height) :after (value (pn projection))
  (when (initialized pn)
    (setf (slot-value pn 'projection-height) (* value (y-scaler pn)))))

(defmethod (setf projection-width) :after (value (pn projection))
  (unless (> (projection-width pn) 0)
    (error "projection::set-projection-width: The projection-width ~
            must be > 0"))
  (when (initialized pn)
    (setf (width pn) (floor (/ value (x-scaler pn))))))

(defmethod (setf projection-height) :after (value (pn projection))
  (unless (> (projection-height pn) 0)
    (error "projection::set-projection-height: The projection-height ~
            must be > 0"))
  (when (initialized pn)
    (setf (height pn) (floor (/ value (y-scaler pn))))))


(defmethod (setf x-scaler) :after (value (pn projection))
  ;;; this does not alter the image dimensions but the projection-width (see
  ;;; description)
  (when (initialized pn)
    (setf (slot-value pn 'projection-width) (* value (width pn)))))

(defmethod (setf y-scaler) :after (value (pn projection))
  (when (initialized pn)
    (setf (slot-value pn 'projection-height) (* value (height pn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod init-x-dimensions ((pn projection))
  (cond ((and (not (x-scaler pn)) (projection-width pn))
         (setf (slot-value pn 'x-scaler) (/ (projection-width pn)
                                            (width pn)))
         t)
        ((and (x-scaler pn) (not (projection-width pn)))
         (setf (slot-value pn 'projection-width) (* (x-scaler pn)
                                                    (width pn))))
        ((and (x-scaler pn) (projection-width pn))
         (let ((test-res (* (width pn) (x-scaler pn))))
           (when (/= test-res (projection-width pn))
             (error "projection::init-x-dimensions: Inconsistent data: ~
                       width * x-scaler != projection-width. ~% ~
                       (~a * ~a != ~a; projection-width should be ~a)."
                    (width pn) (x-scaler pn) (projection-width pn)
                    test-res)))
         t)
        (t nil)))

(defmethod init-y-dimensions ((pn projection))
  (cond ((and (not (y-scaler pn)) (projection-height pn))
         (setf (slot-value pn 'y-scaler) (/ (projection-height pn)
                                            (height pn))))
        ((and (y-scaler pn) (not (projection-height pn)))
         (setf (slot-value pn 'projection-height) (* (y-scaler pn)
                                                     (height pn))))
        ((and (y-scaler pn) (projection-height pn))
         (let ((test-res (* (height pn) (y-scaler pn))))
           (when (/= test-res (projection-height pn))
             (error "projection::init-y-dimensions: Inconsistent data: ~
                       height * y-scaler != projection-height. ~% ~
                       (~a * ~a != ~a; projection-height should be ~a)."
                    (height pn) (y-scaler pn) (projection-height pn)
                    test-res))))
        ;;; try to derive from x-dimensions
        ((numberp (x-scaler pn))
         (setf (slot-value pn 'y-scaler) (x-scaler pn)
               (slot-value pn 'projection-height) (* (y-scaler pn)
                                                     (projection-width pn))))
        (t (error "projection::update: Neither an y-scaler nor a ~
                     projection-height are given."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update :after ((pn projection) &key ignore)
  (declare (ignore ignore))
  ;; update the projection dimensions in case they have been changed
  ;; by a method from the image class
  (when (initialized pn)
    ;; slot value!!
    (setf (slot-value pn 'projection-width) (* (width pn) (x-scaler pn))
          (slot-value pn 'projection-height) (* (height pn) (y-scaler pn))))
  ;; initialize
  (when (and (data pn) (width pn) (height pn) (not (initialized pn)))
    (cond ((init-x-dimensions pn)
           ;; just go ahead
           (init-y-dimensions pn))
          ((or (y-scaler pn) (projection-height pn))
           ;; try to derive the x-dimensions from the y-scaler,
           ;; assuming to keep aspect ratio of image
           (init-y-dimensions pn)
           (setf (slot-value pn 'x-scaler) (y-scaler pn)
                 (slot-value pn 'projection-width) (* (x-scaler pn)
                                                      (width pn))))
          (t (error "projection::update: Neither an x-scaler nor a ~
                     projection-width nor y-dimensions are given.")))
    (setf (slot-value pn 'initialized) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* projection/make-projection
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-02
;;; 
;;; DESCRIPTION
;;; Shorthand to make a projection object.
;;;
;;; A projection is a subclass of an image. Its purpose is to "translate" the
;;; dimensions of an image object to the dimensions of the projection surface.
;;; The data-slot holds the actual (unscaled) image object with its original
;;; dimensions and properties. Its relation to the projection-surface is
;;; determined by the projection-width and -height slots as well as by the
;;; related x- and y-scalers.
;;;
;;; The scalers determine the ratio projection / image (e.g. projection-width /
;;; image-width).
;;; 
;;; NB: Altering the projection-width and -height after initialization will
;;;     resize the image applying the given scaling-factors. Changing the
;;;     scaling factors, though, will not change the dimensions of the image
;;;     but those of the projection. Finally, changing the image dimensions will
;;;     change the projection-width and/or -height.
;;;
;;; NB2: You need to specify at least a value for :projection-width/-height or
;;;      :x-/y-scaler. If both are specified, it needs to be ensured that width
;;;      * x-scaler = projection-width resp.
;;;      height * y-scaler = projection-height remain true.
;;; 
;;;      It is also possible to just specify one coordinate (x/width or
;;;      y/height). Then, the complementary side will be scaled to the same
;;;      scaler as the side where an argument is given (i.e. this keeps the
;;;      aspect ratio of the original image). 
;;;
;;; The coordinate values of projections are -- other than those of images --
;;; not limited to integer values, but can also be e.g. floats.
;;;
;;; ARGUMENTS
;;; - An image object, an imago::image, or the path to an image file. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :x-scaler. A scaler (ratio image-width/projection-width) to link the
;;;   dimensions of the image (in the data slot) to the dimensions of the
;;;   projection.
;;;   NB: This value can be omitted when a :projection-width is given. 
;;; - :y-scaler. A scaler (ratio image-height/projection-height) to link the
;;;   dimensions of the image (in the data slot) to the dimensions of the
;;;   projection.
;;;   NB: This value can be omitted when a :projection-height is given. 
;;; - :projection-width. The width of the image in the projection space.
;;;   NB: If a :x-scaler is given, this value can be omitted, otherweise
;;;   it should be width * x-scaler = projection-width.
;;; - :projection-height. The height of the image in the projection space.
;;;   NB: If a :y-scaler is given, this value can be omitted otherweise, it
;;;   should be height * y-scaler = projection-height.
;;; - :id. The id of the image object.
;;; - :default-interpolation. The default interpolation method (used e.g. when
;;;   changing the image dimensions via the (setf ...) methods.
;;;   Default = (get-apr-config :default-interpolation)
;;; 
;;; RETURN VALUE
;;; The new projection object. 
;;;
;;; EXAMPLE
#|
(let* ((img (make-rgb-image 200 300))
       (pn (make-projection img :x-scaler 2
                                :y-scaler 2)))
  pn)

;; =>
PROJECTION: projection-width: 400, projection-height: 600 
        x-scaler: 2, y-scaler: 2, initialized: T
IMAGE: width: 200, height: 300
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<RGB-IMAGE (200x300) {700ED06133}>
|#
;;; SYNOPSIS
(defmethod make-projection (image &key
                                    x-scaler
                                    y-scaler
                                    projection-width
                                    projection-height
                                    id
                                    (default-interpolation
                                     (get-apr-config :default-interpolation)))
  ;;; ****
  (cond ((typep image 'image) (setf image (data image)))
        ((and (or (pathnamep image) (stringp image))
              (probe-file image))
         (setf image (data (make-image-from-file image)))))
  ;; (when (typep image 'image)
  ;;   (setf image (data image)))
  (make-instance 'projection :x-scaler x-scaler
                             :y-scaler y-scaler
                             :projection-width projection-width
                             :projection-height projection-height
                             :id id
                             :default-interpolation default-interpolation
                             :data image))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod copy ((dest projection) (src projection)
                 &key
                   (dest-x 0)
                   (dest-y 0)
                   (src-x 0)
                   (src-y 0)
                   width height
                   (interpolation
                    (get-apr-config :default-interpolation)))
  ;;; ****
  (let ((src->dest-x (/ (x-scaler src)
                        (x-scaler dest)))
        (src->dest-y (/ (y-scaler src)
                        (y-scaler dest)))
        (tmp-src src))
    ;; scale the src if necessary
    (unless (= 1.0 src->dest-x src->dest-y)
      ;; warn when image is upscaled
      (when (or (< 1.0 src->dest-x) (< 1.0 src->dest-y))
        (warn "projection::copy: The src image will be upscaled by a factor of ~
               x: ~a, y: ~a." src->dest-x src->dest-y))
      ;; clone the src
      (setf tmp-src (make-image (data src)))
      (scale tmp-src src->dest-x src->dest-y :interpolation interpolation))
    (let ((height (when height (round (/ height (y-scaler src)))))
          (width (when width (round (/ width (x-scaler src)))))
          (src-y (round (/ src-y (y-scaler src))))
          (src-x (round (/ src-x (x-scaler src))))
          (dest-y (round (/ dest-y (y-scaler dest))))
          (dest-x (round (/ dest-x (x-scaler dest)))))
      (if (every #'(lambda (x)
                     (or (null x) (< -1 x)))
                 (list height width height src-x src-y))
          (imago::copy (data dest) (data tmp-src)
                       :height height
                       :width width
                       :src-y src-y
                       :src-x src-x
                       :dest-y dest-y
                       :dest-x dest-x)
          (warn "projection::copy: Won't copy. The resulting dimensions are ~
                 too small.")))
    dest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this version does not scale the image but the scalers
(defmethod scale ((pn projection) width-factor height-factor &rest ignore)
  ;;; ****
  (declare (ignore ignore))
  (setf (x-scaler pn) (* width-factor (x-scaler pn))
        (y-scaler pn) (* height-factor (y-scaler pn))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF projection.lisp
