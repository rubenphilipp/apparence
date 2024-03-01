;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/cylinder-mantle
;;; NAME
;;; cylinder-mantle
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-26
;;; 
;;; PURPOSE
;;; This class implements the cylinder mantle projection surface. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> projection-surface -> cylinder-mantle
;;;
;;; $$ Last modified:  19:20:30 Fri Mar  1 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass cylinder-mantle (projection-surface)
  ;; NB: diameter and width should never be assigned at the same time
  ;; NB2: the width in this case is the circumference of cylinder
  ((diameter :accessor diameter :initarg :diameter :initform nil)
   (initialized :accessor initialized :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((cm cylinder-mantle) &rest initargs)
  (declare (ignore initargs))
  (update cm))

(defmethod print-object :before ((cm cylinder-mantle) stream)
  (format stream "~%CYLINDER-MANTLE: diameter: ~a"
          (diameter cm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((cm cylinder-mantle))
  (clone-with-new-class cm 'cylinder-mantle))

(defmethod clone-with-new-class :around ((cm cylinder-mantle) new-class)
  (declare (ignore new-class))
  (let ((new (call-next-method)))
    (setf (slot-value new 'diameter) (diameter cm))
    (setf (slot-value new 'initialized) (initialized cm))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod (setf diameter) :after (value (cm cylinder-mantle))
  (unless (> value 0)
    (error "cylinder-mantle::(setf diameter): The diameter must be > 0"))
  (set-width cm)
  (update cm))

(defmethod (setf width) :after (value (cm cylinder-mantle))
  (unless (> value 0)
    (error "cylinder-mantle::(setf width): The width must be > 0"))
  (set-diameter cm)
  (update cm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-diameter ((cm cylinder-mantle))
  (let ((width (width cm)))
    (setf (slot-value cm 'diameter)
          (/ width pi))))

(defmethod set-width ((cm cylinder-mantle) &rest ignore)
  (declare (ignore ignore))
  ;; NB: this is, in this case, the circumference
  (let ((diameter (diameter cm)))
    (setf (slot-value cm 'width)
          (* diameter pi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update ((cm cylinder-mantle) &key ignore)
  (declare (ignore ignore))
  (unless (initialized cm)
    (if (and (width cm) (diameter cm))
        (error "cylinder-mantle::initialize-instance: You can't ~
                specify both the diameter and the width slots. ~a" cm))
    (let ((diameter (diameter cm))
          (width (width cm)))
      (cond ((and diameter (not width))
             (set-width cm))
            ((and width (not diameter))
             (set-diameter cm))
            ((and width diameter)
             (error "cylinder-mantle::initialize-instance: You can't ~
                   specify both the diameter and the width slots. ~a" cm))))
    (setf (slot-value cm 'initialized) t)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* cylinder-mantle/make-cylinder-mantle
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-26
;;; 
;;; DESCRIPTION
;;; Helper function to instantiate a cylinder-mantle object.
;;; A cylinder mantle is a specialized projection-surface. 
;;; 
;;; Please note that you cannot set both the diameter and the width (i.e. the
;;; circumference) value when instantiating the object.
;;;
;;; NB: The unit for positions/coordinates do not use a specified unit
;;; (e.g. meters or px). This facilitates working on surfaces of an arbitrary
;;; scale without deciding for the final output scale. The conversion, e.g. to
;;; pixel values, will take place when creating a canvas from a projection
;;; surface.
;;;
;;; ARGUMENTS
;;; The height of the cylindrical surface.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :diameter. The diameter of the cylindrical mantle.
;;;   NB: Do not set this value when a :width value is given. 
;;; - :width. The width/circumference of the cylindrical mantle.
;;;   NB: Do not set this value when a :diameter value is given.
;;; - :id. The id of the cylinder.
;;; 
;;; RETURN VALUE
;;; The new cylinder-mantle object. 
;;;
;;; EXAMPLE
#|
(make-cylinder-mantle 47 :diameter 38 :id 'visiodrom)
;; =>
CYLINDER-MANTLE: diameter: 38
PROJECTION-SURFACE: width: 119.38052083641213d0, height: 47
NAMED-OBJECT: id: VISIODROM, tag: NIL, 
data: NIL
|#
;;; SYNOPSIS
(defun make-cylinder-mantle (height &key diameter width id)
  ;;; ****
  (make-instance 'cylinder-mantle :height height
                                  :width width
                                  :diameter diameter
                                  :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* cylinder-mantle/get-coordinates
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; This method returns the cartesian coordinates of a point on the
;;; cylinder mantle. The location of the point is given as the azimuth angle (in
;;; degrees clockwise) from a given azimuth origin. By default, the origin is
;;; placed at the top of the circle.  The azimuth origin can be adjusted
;;; (clockwise) by an azimuth-offset value.
;;;
;;; ARGUMENTS
;;; - A cylinder-mantle object.
;;; - The azimuth (in degrees, clockwise) of the point on the circle, the
;;;   x,y coordinates should be returned. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :azimuth-origin-offset. A number that indicates the offset of the azimuth
;;;   origin on the circular base of the cylindrical mantle (in degrees,
;;;   clockwise; see above).  Default = 0
;;; - :center-x. The x-axis position of the center of the circle.  Default = 0
;;; - :center-y. The y-axis position of the center of the circle.  Default = 0
;;; - :round-to-digits. An integer indicating the number of digits the result
;;;   should be (f)rounded to. When NIL, the result will not be rounded.
;;;   Default = 3
;;; 
;;; RETURN VALUE
;;; A list with the x and y coordinates of the location of the point.
;;;
;;; EXAMPLE
#|
(let ((cm (make-cylinder-mantle 10 :diameter 10)))
  (get-coordinates cm 90 :azimuth-origin-offset 0))
;; => (5.0d0 0.0d0)
|#
;;; SYNOPSIS
(defmethod get-coordinates ((cm cylinder-mantle) azimuth
                            &key
                              (azimuth-origin-offset 0)
                              (center-x 0)
                              (center-y 0)
                              (round-to-digits 3))
;;; ****
  (unless (or (null round-to-digits)
              (integerp round-to-digits))
    (error "cylinder-mantle::get-coordinates: The value of :round-to-digits ~
            must be either NIL or an integer. "))
  (let* ((radius (/ (diameter cm) 2))
         (azimuth-w-offset (- (+ azimuth azimuth-origin-offset) 90))
         (x (+ (* radius (cos (degrees->radians azimuth-w-offset)))
               center-x))
         (y (+ (* radius (sin (degrees->radians azimuth-w-offset)))
               center-y)))
    (when round-to-digits
      (setf x (fround-to-digits x round-to-digits))
      (setf y (fround-to-digits y round-to-digits)))
    (list x y)))
             


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cylinder-mantle.lisp
