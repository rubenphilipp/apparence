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
;;; $$ Last modified:  17:00:56 Sat Mar  2 2024 CET
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

(defmethod set-width ((pn projection) &rest ignore)
  (declare (ignore ignore))
  ;; changing the width alters the projection-width
  (setf (slot-value pn 'projection-width) (* (width pn) (x-scaler pn))))

(defmethod set-height ((pn projection) &rest ignore)
  (declare (ignore ignore))
  (setf (slot-value pn 'projection-height) (* (height pn) (y-scaler pn))))

(defmethod set-projection-width ((pn projection) &rest ignore)
  (declare (ignore ignore))
  (unless (> (projection-width pn) 0)
    (error "projection::set-projection-width: The projection-width ~
            must be > 0"))
  ;;; this does not alter the image dimensions (see description)
  (setf (width pn) (/ (projection-width pn) (x-scaler pn))))

(defmethod set-projection-height ((pn projection) &rest ignore)
  (declare (ignore ignore))
  (unless (> (projection-height pn) 0)
    (error "projection::set-projection-height: The projection-height ~
            must be > 0"))
  (setf (height pn) (/ (projection-height pn) (y-scaler pn))))

(defmethod set-x-scaler ((pn projection) &rest ignore)
  (declare (ignore ignore))
  ;;; this does not alter the image dimensions but the projection-width (see
  ;;; description)
  (setf (slot-value pn 'projection-width) (* (x-scaler pn) (width pn))))

(defmethod set-y-scaler ((pn projection) &rest ignore)
  (declare (ignore ignore))
  (setf (slot-value pn 'projection-height) (* (y-scaler pn) (height pn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf width) :after (value (pn projection))
  (declare (ignore value))
  (when (initialized pn)
    (set-width pn)))

(defmethod (setf height) :after (value (pn projection))
  (declare (ignore value))
  (when (initialized pn)
    (set-height pn)))

(defmethod (setf projection-width) :after (value (pn projection))
  (declare (ignore value))
  (when (initialized pn)
    (set-projection-width pn)))

(defmethod (setf projection-height) :after (value (pn projection))
  (declare (ignore value))
  (when (initialized pn)
    (set-projection-height pn)))

(defmethod (setf x-scaler) :after (value (pn projection))
  (declare (ignore value))
  (when (initialized pn)
    (set-x-scaler pn)))

(defmethod (setf y-scaler) :after (value (pn projection))
  (declare (ignore value))
  (when (initialized pn)
    (set-y-scaler pn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update :after ((pn projection) &key ignore)
  (declare (ignore ignore))
  (when (and (projection-width pn) (projection-height pn)
             (x-scaler pn) (y-scaler pn))
    (setf (slot-value pn 'initialized) t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF projection.lisp
