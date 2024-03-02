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
;;; $$ Last modified:  19:02:39 Sat Mar  2 2024 CET
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

;; must be called :before, since update will be called when updating the image
;; class, thus, the consistency check won't take the changed dimensions into
;; account
;; RP  Sat Mar  2 17:51:55 2024
(defmethod (setf width) :after (value (pn projection))
  ;; changing the width alters the projection-width
  (print (width pn))
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
    ;; taking care of the width
    (cond ((and (not (x-scaler pn)) (projection-width pn))
           (setf (slot-value pn 'x-scaler) (/ (projection-width pn)
                                              (width pn))))
          ((and (x-scaler pn) (not (projection-width pn)))
           (setf (slot-value pn 'projection-width) (* (x-scaler pn)
                                                      (width pn))))
          ((and (x-scaler pn) (projection-width pn))
           (let ((test-res (* (width pn) (x-scaler pn))))
             (when (/= test-res (projection-width pn))
               (error "projection::update: Inconsistent data: ~
                       width * x-scaler != projection-width. ~% ~
                       (~a * ~a != ~a; projection-width should be ~a)."
                      (width pn) (x-scaler pn) (projection-width pn)
                      test-res))))
          (t (error "projection::update: Neither an x-scaler nor a ~
                     projection-width are given.")))
    ;; now doing the height
    (cond ((and (not (y-scaler pn)) (projection-height pn))
           (setf (slot-value pn 'y-scaler) (/ (projection-height pn)
                                              (height pn))))
          ((and (y-scaler pn) (not (projection-height pn)))
           (setf (slot-value pn 'projection-height) (* (y-scaler pn)
                                                      (height pn))))
          ((and (y-scaler pn) (projection-height pn))
           (let ((test-res (* (height pn) (y-scaler pn))))
             (when (/= test-res (projection-height pn))
               (error "projection::update: Inconsistent data: ~
                       height * y-scaler != projection-height. ~% ~
                       (~a * ~a != ~a; projection-height should be ~a)."
                      (height pn) (y-scaler pn) (projection-height pn)
                      test-res))))
          (t (error "projection::update: Neither an y-scaler nor a ~
                     projection-height are given.")))
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
;;; NB2: You need to specify at least a value for :projection-width/-height
;;;      or :x-/y-scaler. If both are specified, it needs to be ensured that
;;;      width * x-scaler = projection-width resp.
;;;      height * y-scaler = projection-height remain true. 
;;;
;;; The coordinate values of projections are -- other than those of images --
;;; not limited to integer values, but can also be e.g. floats.
;;;
;;; ARGUMENTS
;;; - An image object or an imago::image.
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
  (when (typep image 'image)
    (setf image (data image)))
  (make-instance 'projection :x-scaler x-scaler
                             :y-scaler y-scaler
                             :projection-width projection-width
                             :projection-height projection-height
                             :id id
                             :default-interpolation default-interpolation
                             :data image))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF projection.lisp
