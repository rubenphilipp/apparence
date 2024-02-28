;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/canvas
;;; NAME
;;; canvas
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; PURPOSE
;;; Implementation of the canvas class. A canvas is (according to the metaphor
;;; linked to vis-arts) a surface other visual items will be added to.
;;; Essentially, it holds an imago (rgb) image object in the data slot, which
;;; is created during the initialization process with the given dimensions and
;;; could be altered via several methods. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> canvas
;;;
;;; $$ Last modified:  23:08:18 Wed Feb 28 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass canvas (named-object)
  ;; measures in px
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   ;; The rgb(a)-background color of the canvas, as a list.
   ;; Default = (0 0 0 0)
   (color :accessor color :initarg :color :initform '(0 0 0 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((cv canvas) &rest initargs)
  (declare (ignore initargs))
  (unless (and (numberp (width cv)) (> (width cv) 0))
    (error "canvas::initialize-instance: The width is not a number > 0"))
  (unless (and (numberp (height cv)) (> (height cv) 0))
    (error "canvas::initialize-instance: The height is not a number > 0"))
  (update cv))

(defmethod print-object :before ((cv canvas) stream)
  (format stream "~%CANVAS: width: ~a, height: ~a, color: ~a"
          (width cv) (height cv)
          (let ((c (color cv)))
            (list (color-red c) (color-green c) (color-blue c)
                  (color-alpha c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf color) :after (value (cv canvas))
  (declare (ignore value))
  (when (data cv)
    (error "canvas::(setf color): You can't change the color after ~
            instantiation as this would delete the content of the canvas."))
  (set-color cv))

(defmethod set-color ((cv canvas) &rest ignore)
  (declare (ignore ignore))
  (let ((c (color cv)))
    (unless (or (rgb-p c) (rgba-p c))
      (error "canvas::(setf color): The :color is not a valid color list"))
    (setf (slot-value cv 'color) (apply #'make-color c))))


(defmethod (setf width) :after (value (cv canvas))
  (declare (ignore value))
  (set-width cv))

(defmethod set-width ((cv canvas) &rest ignore)
  (declare (ignore ignore))
  (unless (and (numberp (width cv)) (> (width cv) 0))
    (error "canvas::set-width: The width is not a number > 0"))
  (warn "canvas::set-width: Altering the width of an existing canvas might ~
         cause the cutting of existing content.")
  (let ((new (make-rgb-image (width cv) (height cv) (color cv))))
    (copy new (data cv))
    (setf (slot-value cv 'data) new)))

(defmethod (setf height) :after (value (cv canvas))
  (declare (ignore value))
  (set-height cv))

(defmethod set-height ((cv canvas) &rest ignore)
  (declare (ignore ignore))
  (unless (and (numberp (height cv)) (> (height cv) 0))
    (error "canvas::set-height: The height is not a number > 0"))
  (warn "canvas::set-height: Altering the height of an existing canvas might ~
         cause the cutting of existing content.")
  (let ((new (make-rgb-image (width cv) (height cv) (color cv))))
    (copy new (data cv))
    (setf (slot-value cv 'data) new)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update ((cv canvas) &key ignore)
  (declare (ignore ignore))
  ;; when data is not an image, initialize
  (unless (typep (data cv) 'image)
    (set-color cv)
    ;; initialize canvas data
    (setf (slot-value cv 'data) (make-rgb-image (width cv) (height cv)
                                                (color cv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* canvas/make-canvas
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; This function creates a canvas object. 
;;;
;;; ARGUMENTS
;;; - The width of the canvas (number).
;;; - The height of the canvas (number). 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :color. A three- or four-item list of rgb(a) values determining the canvas
;;;   background color.  Default = '(0 0 0 0)
;;; - :id. The id of the canvas. 
;;; 
;;; RETURN VALUE
;;; The canvas object. 
;;;
;;; EXAMPLE
#|
(make-canvas 100 200)
;; =>
CANVAS: width: 100, height: 200, color: (0 0 0 0)
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<RGB-IMAGE (100x200) {70170E8EA3}>
|#
;;; SYNOPSIS
(defun make-canvas (width height &key
                                   (color '(0 0 0 0))
                                   id)
  ;;; ****
  (make-instance 'canvas :width width
                         :height height
                         :color color
                         :id id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* canvas/write-png
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; This method writes a canvas to a png-file. 
;;;
;;; ARGUMENTS
;;; A canvas object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :outfile. The output location for the png-file.
;;;   Default = "/tmp/canvas.png"
;;; 
;;; RETURN VALUE
;;; cf. imago::write-png
;;;
;;; EXAMPLE
#|
(let ((cv (make-instance 'canvas :width 300 :height 200 :color '(255 255 255)))
      (img (make-rgb-image 50 100 (make-color 100 233 90))))
  (copy (data cv) img)
  (write-png cv :outfile "~/Downloads/cv-test.png"))
|#
;;; SYNOPSIS
(defmethod write-png ((cv canvas) &key (outfile "/tmp/canvas.png"))
  ;;; ****
  (let ((img (data cv)))
    (imago::write-png img outfile)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF canvas.lisp
