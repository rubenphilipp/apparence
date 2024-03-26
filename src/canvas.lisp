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
;;; Essentially, it holds an image object in the data slot, which is created
;;; during the initialization process with the given dimensions and could be
;;; altered via several methods.
;;;
;;; CLASS HIERARCHY
;;; named-object -> canvas
;;;
;;; $$ Last modified:  00:27:42 Wed Mar 27 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass canvas (named-object)
  ;; measures in px
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   ;; The rgb(a)-background color of the canvas, as a list.
   ;; Default = (0 0 0 0)
   (color :accessor color :initarg :color :initform '(0 0 0 0))
   (initialized :accessor initialized :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((cv canvas) &rest initargs)
  (declare (ignore initargs))
  (update cv))

(defmethod print-object :before ((cv canvas) stream)
  (format stream "~%CANVAS: width: ~a, height: ~a, color: ~a, ~
                  ~%        initialized: ~a"
          (width cv) (height cv) (color->list (color cv)) (initialized cv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((cv canvas))
  (clone-with-new-class cv 'canvas))

(defmethod clone-with-new-class :around ((cv canvas) new-class)
  (declare (ignore new-class))
  (let ((new (call-next-method)))
    (setf (slot-value new 'width) (width cv)
          (slot-value new 'height) (height cv)
          (slot-value new 'color) (color cv)
          (slot-value new 'initialized) (initialized cv)
          ;; the data slot needs to be cloned as this is an image object
          (slot-value new 'data) (clone (data cv)))
    new))

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
    ;; just convert color list when a list is given
    (when (or (rgb-p c) (rgba-p c))
        (setf (slot-value cv 'color) (apply #'make-color c)))))


(defmethod (setf width) :after (value (cv canvas))
  (declare (ignore value))
  (set-width cv))

(defmethod set-width ((cv canvas) &rest ignore)
  (declare (ignore ignore))
  (unless (and (numberp (width cv)) (> (width cv) 0))
    (error "canvas::set-width: The width is not a number > 0"))
  (when (get-apr-config :verbose)
    (warn "canvas::set-width: Altering the width of an existing canvas might ~
           cause the cutting of existing content."))
  (let ((new (make-rgb-image (width cv) (height cv)
                             :initial-color (color cv))))
    (copy new (data cv))
    (setf (slot-value cv 'data) new)))

(defmethod (setf height) :after (value (cv canvas))
  (declare (ignore value))
  (set-height cv))

(defmethod set-height ((cv canvas) &rest ignore)
  (declare (ignore ignore))
  (unless (and (numberp (height cv)) (> (height cv) 0))
    (error "canvas::set-height: The height is not a number > 0"))
  (when (get-apr-config :verbose)
    (warn "canvas::set-height: Altering the height of an existing canvas might ~
           cause the cutting of existing content."))
  (let ((new (make-rgb-image (width cv) (height cv)
                             :initial-color (color cv))))
    (copy new (data cv))
    (setf (slot-value cv 'data) new)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update ((cv canvas) &key ignore)
  (declare (ignore ignore))
  ;;(print "cv")
  ;; when data is not an image, initialize
  (unless (typep (data cv) 'image)
    (when (color cv) (set-color cv))
    ;; initialize canvas data when width and height are given
    (when (and (width cv) (height cv) (color cv))
      (setf (slot-value cv 'data)
            (make-rgb-image (width cv) (height cv)
                            :initial-color (color cv))
            (slot-value cv 'initialized) t))))

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
data: 
IMAGE: width: 100, height: 200
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<RGB-IMAGE (100x200) {700EE3E293}>
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
;;; The path to the output file. 
;;;
;;; EXAMPLE
#|
(let ((cv (make-instance 'canvas :width 300 :height 200 :color '(55 255 55)))
      (img (make-rgb-image 50 100)))
  (copy (data cv) img)
  (write-png cv :outfile "~/Downloads/cv-test.png"))
|#
;;; SYNOPSIS
(defmethod write-png ((cv canvas) &key (outfile "/tmp/canvas.png"))
  ;;; ****
  (unless (initialized cv)
    (error "canvas::write-png: The canvas object has not been initialized."))
  (let ((img (data cv)))
    (write-png img :outfile outfile))
  outfile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* canvas/write-jpg
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; This method writes a canvas to a jpg-file. 
;;;
;;; ARGUMENTS
;;; A canvas object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :outfile. The output location for the png-file.
;;;   Default = "/tmp/canvas.jpg"
;;; - :quality. The quality of the output as an integer (0<=q<=100), where 100
;;;   means best quality. Default = 100.
;;; 
;;; RETURN VALUE
;;; Returns the output file (string). 
;;;
;;; EXAMPLE
#|
(let ((cv (make-instance 'canvas :width 300 :height 200 :color '(55 255 55)))
      (img (make-rgb-image 50 100)))
  (copy (data cv) img)
  (write-jpg cv :outfile "~/Downloads/cv-test.jpg"))
|#
;;; SYNOPSIS
(defmethod write-jpg ((cv canvas) &key
                                    (outfile "/tmp/canvas.jpg")
                                    (quality 100))
  ;;; ****
  (unless (initialized cv)
    (error "canvas::write-jpg: The canvas object has not been initialized."))
  (write-jpg (data cv) :outfile outfile :quality quality)
  outfile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* canvas/put-it
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; This method puts an (imago) image object on top of the canvas by applying a
;;; compositing function (compose-fun) to both images. 
;;;
;;; ARGUMENTS
;;; - The canvas object.
;;; - The image object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :height. The height of the image object on the canvas. If non-NIL, the
;;;   image will be cropped to the given height (in px).
;;; - :width. The width of the image object on the canvas. If non-NIL, the
;;;   image will be cropped to the given width (in px).
;;; - :src-y. The y coordinate within the image object used when cropped as the
;;;   left origin. Default = 0
;;; - :src-x. The x coordinate within the image object used when cropped as the
;;;   top origin. Default = 0
;;; - :dest-y. The y coordinate of the location the image object will be placed
;;;   on the canvas. Default = 0
;;; - :dest-x. The x coordinate of the location the image object will be placed
;;;   on the canvas. Default = 0
;;; - :compose-fun. The default function for performing the compositing.
;;;   Default = #'apr-default-compose-op, which is the Porter/Duff A over B
;;;   algorithm. 
;;; 
;;; RETURN VALUE
;;; The canvas object. 
;;;
;;; EXAMPLE
#|
(let ((cv (make-canvas 300 200 :color '(255 255 255 0)))
      (img (make-rgb-image 50 100 :initial-color (make-color 100 233 90)))
      (img2 (make-rgb-image 50 100 :initial-color (make-color 80 133 90))))
  (put-it cv img :dest-x 0)
  (put-it cv img2 :dest-x 20)
  (write-png cv :outfile "~/Downloads/cv-test.png"))
|#
;;; SYNOPSIS
(defmethod put-it ((cv canvas) (img image)
                   &key
                     height
                     width
                     (src-y 0)
                     (src-x 0)
                     (dest-y 0)
                     (dest-x 0)
                     (compose-fun #'apr-default-compose-op))
  ;;; ****
  (unless (initialized cv)
    (error "canvas::put-it: The canvas object has not been initialized."))
  (setf (data cv)
        (put-it (data cv) img
                :dest-x dest-x :dest-y dest-y
                :src-x src-x :src-y src-y
                :width width :height height
                :compose-fun compose-fun))
  cv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* canvas/put-it-circular
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-29
;;; 
;;; DESCRIPTION
;;; This method puts an (imago) image onto a circularly folded canvas (i.e. the
;;; lateral surface / mantle of a cylinder). The circular position/azimuth of
;;; the source image can be defined by a degree offset from the center-origin on
;;; the canvas (which is relative to the width of the unfolded, flat mantle).
;;;
;;; ARGUMENTS
;;; - The canvas object.
;;; - The image object to be projected. 
;;; - The azimuth angle for the projection.
;;; - The vertical position of the image relative to the top of the canvas. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :height. The height of the src-image. When NIL, the original height will
;;;   be used. Default = NIL
;;; - :width. The width of the src-image. When NIL, the original width will be
;;;   used. Default = NIL
;;; - :src-y. When a :height is given, this determines to top-offset in the src.
;;;   Default = 0
;;; - :src-x. When a width is given, this determines the left-offset in the src.
;;;   Default = 0
;;; - :canvas-origin. The horizontal position of the origin (i.e. the center) on
;;;   the canvas. This value is relative to the width of the canvas and must be
;;;   a number between 0.0 and 1.0.  Default = 0.0
;;; - :image-origin. The horizontal position of the origin on the image to be
;;;   projected. This value is relative to the image width and must be a number
;;;   between 0.0 and 1.0.  Default = 0.5
;;; - :verbose. A boolean value. When T, then some information is printed during
;;;   the process.  Default = (get-apr-config :verbose)
;;; - :compose-fun. The default compositing function used to derive the color
;;;   for each pixel. This function must take two arguments:
;;;   - The dest-color (imago-color)
;;;   - The src-color (imago-color)
;;;   And must return an imago-color (cf. apr-default-compose-op, as well as
;;;   compositing.lisp and imago.lisp).
;;;   Default = #'apr-default-compose-op
;;; 
;;; RETURN VALUE
;;; The modified canvas object. 
;;;
;;; EXAMPLE
#|
(let* ((cv (make-canvas 4000 2000 :color '(0 0 0 0)))
       (img (make-rgb-image 500 400 :initial-color (make-color 233 200 188))))
  (put-it-circular cv img 350 0 :width 300 :height 200
                                :src-y 10 :src-x 20)
  (write-png cv :outfile "/tmp/test.png")
  (system-open-file "/tmp/test.png"))
|#
;;; SYNOPSIS
(defmethod put-it-circular ((cv canvas) (image image) azimuth y
                            &key
                              height
                              width
                              (src-y 0)
                              (src-x 0)
                              (canvas-origin 0.0)
                              (image-origin 0.5)
                              (compose-fun #'apr-default-compose-op)
                              (verbose (get-apr-config :verbose)))
  ;;; ****
  (unless (initialized cv)
    (error "canvas::put-it-circular: The canvas object has not been ~
            initialized."))
  (unless (and (<= 0.0 image-origin) (>= 1.0 image-origin))
    (error "canvas::put-it-circular: The image-origin must be ~
            >= 0.0 and <= 1.0"))
  (unless (and (<= 0.0 canvas-origin) (>= 1.0 canvas-origin))
    (error "canvas::put-it-circular: The canvas-origin must be ~
            >= 0.0 and <= 1.0"))
  (when (>= y (height cv))
    (error "canvas::put-it-circular: The y-coordinate is >= the height of ~
            the canvas."))
  ;; (unless (typep image 'image)
  ;;   (error "canvas::put-it-circular: The image is not an image, but ~a"
  ;;          (type-of image)))
  (when (or height width (/= 0 src-y) (/= 0 src-x))
    (let ((new-height (if height height (height image)))
          (new-width (if width width (width image)))
          (img-tmp (clone image)))
      ;;; cloning is necessary, otherwise the original image will be overwritten
      ;;; RP  Fri Mar  1 18:30:38 2024
      (crop img-tmp src-x src-y new-width new-height)
      (setf image img-tmp)))
  (let* (;;(canvas (data cv))
         (canvas cv)
         (canvas-width (width cv)) ;;(width (data cv)))
         (img-width (width image))
         ;; anchor point (x-axis from left) on image, i.e. the origin x-coords
         (img-anchor (floor (* img-width image-origin)))
         (rotation-angle (mod azimuth 360))
         ;; the absolute canvas origin
         (canvas-anchor (floor (* canvas-origin canvas-width)))
         ;; rotation origin on the canvas
         (rotation-origin (floor (+ (* (/ canvas-width 360)
                                       rotation-angle)
                                    canvas-anchor)))
         ;; the x-coordinates for the image on the canvas (x-left x-right)
         (image-x-coords (list
                          ;; left
                          (- rotation-origin img-anchor)
                          ;; right
                          (+ rotation-origin
                             (- img-width img-anchor)))))
    ;; project the image onto the cylindrical plane / mantle
    ;; in case there is an overlap on either the left or right side of the
    ;; canvas, wrap the image
    (cond
      ((and (<= 0 (first image-x-coords))
            (>= canvas-width (second image-x-coords)))
       (when verbose
         (format t "canvas::put-it-circular: no-split~%"))
       (put-it canvas image :dest-x (first image-x-coords)
                            :dest-y y
                            :compose-fun compose-fun))
      ((and (> 0 (first image-x-coords))
            (>= canvas-width (second image-x-coords)))
       (when verbose
         (format t "canvas::put-it-circular: left->right~%"))
       ;; wrap left->right
       ;; starting with the "left" part (i.e. the right part of the img)
       (put-it canvas image :width (second image-x-coords)
                            :src-x (abs (first image-x-coords))
                            :dest-y y
                            :compose-fun compose-fun)
       ;; now the "right" part
       (put-it canvas image :width (abs (first image-x-coords))
                            :src-x 0
                            :dest-x (+ canvas-width
                                     (first image-x-coords))
                            :dest-y y
                            :compose-fun compose-fun))
      ((and (<= 0 (first image-x-coords))
            (< canvas-width (second image-x-coords)))
       (when verbose
         (format t "canvas::put-it-circular: right->left~%"))
       ;; wrap right->left
       ;; starting with the right part (the left part of the img)
       (put-it canvas image :width (- canvas-width
                                      (first image-x-coords))
                            :src-x 0
                            :dest-x (first image-x-coords)
                            :dest-y y
                            :compose-fun compose-fun)
       ;; now the left part
       (put-it canvas image :width (- (second image-x-coords)
                                      canvas-width)
                            :src-x (- canvas-width
                                      (first image-x-coords))
                            :dest-x 0 :dest-y y
                            :compose-fun compose-fun))
      (t (error "the width of the image to be projected is greater than ~
                 the width of the projection screen.")))
    ;; finally, return the altered canvas object
    cv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF canvas.lisp
