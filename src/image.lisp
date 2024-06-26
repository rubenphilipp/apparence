;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/image
;;; NAME
;;; image
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-29
;;; 
;;; PURPOSE
;;; Implementation of the image class. This class holds in its data-slot
;;; an imago::image and provides an interface for accessing relevant attributes.
;;;
;;; NB: This abstraction of the imago::image class is necessary in order to
;;;     provide convenient facilities of referencing images (e.g. to hanbdle
;;;     computing ressources economically when working with a large amount of
;;;     images).
;;;
;;; CLASS HIERARCHY
;;; named-object -> image
;;;
;;; $$ Last modified:  00:41:31 Thu Jun 27 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass image (named-object)
  ;; note: the actual imago::image is stored to the data-slot
  ((width :accessor width :initform nil)
   (height :accessor height :initform nil)
   ;;; the default interpolation method used when changing width or height
   ;;; cf. https://quickref.common-lisp.net/imago.html#index-resize
   ;;; for a list of available interpolation methods
   ;;; when NIL, the imago:*default-interpolation* is used
   (default-interpolation :accessor default-interpolation
                          :initarg :default-interpolation
                          :initform (get-apr-config :default-interpolation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((img image) &rest initargs)
  (declare (ignore initargs))
  (update img))

(defmethod print-object :before ((img image) stream)
  (format stream "~%IMAGE: width: ~a, height: ~a"
          (width img) (height img)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((img image))
  (clone-with-new-class img 'image))

(defmethod clone-with-new-class :around ((img image) new-class)
  (declare (ignore new-class))
  (let ((new (call-next-method)))
    (setf (slot-value new 'width) (width img)
          (slot-value new 'height) (height img)
          (slot-value new 'default-interpolation) (default-interpolation img)
          ;;(slot-value new 'data) (data img)
          (slot-value new 'data) (clone-image (data img))
          )
    new))

;; this needs to be done, as otherwise the same image might be used by cloned
;; instances of other methods
;; RP  Sun Mar  3 23:08:21 2024
(defmethod clone-image ((img imago::image))
  (let* ((w (imago::image-width img))
         (h (imago::image-height img))
         (new (imago::make-rgb-image w h)))
    (imago::copy new img)
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf data) :after (value (img image))
  (declare (ignore value))
  (update img))

(defmethod (setf width) :after (value (img image))
  (resize img value (height img) :interpolation (default-interpolation img)))

(defmethod (setf height) :after (value (img image))
  (resize img (width img) value :interpolation (default-interpolation img)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod update ((img image) &key ignore)
  (declare (ignore ignore))
  (when (data img)
    (unless (typep (data img) 'imago::image)
      (error "image::update: The data-slot of the image object must contain a ~
              imago::image, not a ~a." (type-of (data img))))
    ;;; set width and height from the imago::image
    (let ((image (data img)))
      (setf (slot-value img 'width) (imago::image-width image)
            (slot-value img 'height) (imago::image-height image)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make-functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* image/make-image
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-29
;;; 
;;; DESCRIPTION
;;; This function instantiates an image object from the given data. 
;;;
;;; ARGUMENTS
;;; - An imago::image object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the image object.
;;; - :default-interpolation. The default interpolation method (used e.g. when
;;;   changing the image dimensions via the (setf ...) methods.
;;;   Default = (get-apr-config :default-interpolation)
;;; 
;;; RETURN VALUE
;;; An image object. 
;;;
;;; SYNOPSIS
(defun make-image (data &key id
                          (default-interpolation
                           (get-apr-config :default-interpolation)))
  ;;; ****
  (make-instance 'image :data data
                        :id id
                        :default-interpolation default-interpolation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* image/make-rgb-image
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-29
;;; 
;;; DESCRIPTION
;;; This function creates a new imago::rgb-image. 
;;;
;;; ARGUMENTS
;;; - the width of the image
;;; - the height of the image
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :initial-color. The initial/background color of the new rgb-image.
;;;   Default = (get-apr-config :default-rgb)
;;; - :id. The id of the new image object.
;;; - :default-interpolation. The default interpolation method (used e.g. when
;;;   changing the image dimensions via the (setf ...) methods.
;;;   Default = (get-apr-config :default-interpolation)
;;; 
;;; RETURN VALUE
;;; The new image object. 
;;;
;;; SYNOPSIS
(defun make-rgb-image (width height
                       &key
                         (initial-color
                          (get-apr-config :default-rgb))
                         id
                         (default-interpolation
                          (get-apr-config :default-interpolation)))
  ;;; ****
  (let ((data (imago::make-rgb-image width height
                                     initial-color)))
    (make-image data :id id
                     :default-interpolation default-interpolation)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-image-from-jpg (path &key
                             id
                             (default-interpolation
                              (get-apr-config :default-interpolation)))
  ;;; ****
  (let ((data (imago-jpeg-turbo::read-jpg path)))
    (make-image data :id id
                     :default-interpolation default-interpolation)))


(defun make-image-from-png (path &key
                             id
                             (default-interpolation
                              (get-apr-config :default-interpolation)))
  ;;; ****
  (let ((data (imago-pngio::read-png path)))
    (make-image data :id id
                     :default-interpolation default-interpolation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* image/make-image-from-file
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-29
;;; 
;;; DESCRIPTION
;;; This function makes an image object from an image file.
;;; Currently supported are png and jpg/jpeg images. 
;;;
;;; ARGUMENTS
;;; The path to the image file. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :id. The id of the new image object.
;;; - :default-interpolation. The default interpolation method (used e.g. when
;;;   changing the image dimensions via the (setf ...) methods.
;;;   Default = (get-apr-config :default-interpolation)
;;; 
;;; RETURN VALUE
;;; The new image object. 
;;;
;;; SYNOPSIS
(defun make-image-from-file (path &key
                              id
                              (default-interpolation
                               (get-apr-config :default-interpolation)))
  ;;; ****
  (let ((type (pathname-type path)))
    (cond ((equal "png" type)
           (make-image-from-png path
                                :id id
                                :default-interpolation default-interpolation))
          ((or (equal "jpg" type)
               (equal "jpeg" type))
           (make-image-from-jpg path
                                :id id
                                :default-interpolation default-interpolation))
          (t (error "image::make-image-from-file: The file type ~a is not ~
                     supported." type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* image/make-image-from-svg
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; DESCRIPTION
;;; This method creates an image object from a cl-svg::svg-toplevel object
;;; (cf. svg.lisp). 
;;;
;;; ARGUMENTS
;;; The cl-svg::svg-toplevel object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the new image object.
;;; - :width. The width of the image (in px). Cf. svg->png.
;;; - :height. The height of the image (in px). Cf. svg->png.
;;; - :tmp-dir. A path to a directory where the temp-files are stored.
;;;   Default = (get-apr-config :default-tmp-dir)
;;; - :default-interpolation. The default interpolation method (used e.g. when
;;;   changing the image dimensions via the (setf ...) methods.
;;;   Default = (get-apr-config :default-interpolation)
;;; 
;;; RETURN VALUE
;;; 
;;;
;;; EXAMPLE
#|
(let ((canvas (cl-svg::make-svg-toplevel 'cl-svg:svg-1.1-toplevel
:width 400
:height 300))
(image nil))
(cl-svg:draw canvas
(:rect :x 10 :y 10
:width 100 :height (* 100 4/3)
:fill "rgba(90,90,90,1)"))
(setf image (make-image-from-svg canvas))
(write-png image :outfile "/tmp/image.png")
(system-open-file "/tmp/image.png"))
|#
;;; SYNOPSIS
(defun make-image-from-svg (svg &key
                                  id
                                  width
                                  height
                                  (tmp-dir (get-apr-config :default-tmp-dir))
                                  (default-interpolation
                                   (get-apr-config :default-interpolation)))
  ;;; ****
  (unless (typep svg 'cl-svg::svg-toplevel)
    (error "image::make-image-from-svg: The svg is not an cl-svg::svg-toplevel ~
            object."))
  (let ((tmp-png (format nil "~a~a.png"
                         (trailing-slash tmp-dir)
                         (get-random-uuid)))
        (image nil))
    (svg->png svg :width width
                  :height height
                  :tmp-dir tmp-dir
                  :outfile tmp-png)
    (setf image (make-image-from-file
                 tmp-png
                 :id id
                 :default-interpolation default-interpolation))
    ;; removing garbage
    (delete-file tmp-png)
    image))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These methods reflect some imago methods.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* image/resize
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-01
;;; 
;;; DESCRIPTION
;;; This method resizes an image object to the new dimensions. 
;;;
;;; ARGUMENTS
;;; - The image object.
;;; - A number representing the new height (px).
;;; - A number representing the new width (px). 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :interpolation. The interpolation method to be used for altering the
;;;   dimensions. Default = (get-apr-config :default-interpolation)
;;; 
;;; RETURN VALUE
;;; The modified image object. 
;;;
;;; EXAMPLE
#|
(let ((img (make-rgb-image 200 300)))
(resize img 400 400))
;;; =>
IMAGE: width: 400, height: 400
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<RGB-IMAGE (400x400) {70067BAB33}>
|#
;;; SYNOPSIS
(defmethod resize ((img image) new-width new-height
                   &key (interpolation
                         (get-apr-config :default-interpolation)))
  ;;; ****
  (setf (data img) (imago::resize (data img) new-width new-height
                                  :interpolation interpolation))
  img)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; RP  Thu Jun 27 00:41:31 2024
(defmethod crop ((img image) x y width height)
;;; ****
  (setf (data img)
        (imago::crop (data img) x y width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* image/scale
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-01
;;; 
;;; DESCRIPTION
;;; This methods scales an image object according to the scaling facors given as
;;; arguments. 
;;;
;;; ARGUMENTS
;;; - The image object.
;;; - A number being the width-scaler.
;;; - A number being the height-scaler. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :interpolation. The interpolation method to be used for altering the
;;;   dimensions. Default = (get-apr-config :default-interpolation)
;;; 
;;; RETURN VALUE
;;; The modified image object. 
;;;
;;; EXAMPLE
#|
(let ((img (make-rgb-image 200 300)))   ; ; ; ;
(scale img 1.5 2.0))                    ; ; ; ;
;;; =>                                  ; ; ; ;
IMAGE: width: 300, height: 600          ; ; ; ;
NAMED-OBJECT: id: NIL, tag: NIL,        ; ; ; ;
data: #<RGB-IMAGE (300x600) {7006A8E413}> ; ; ; ;
**********                              ; ; ; ;
|#
;;; SYNOPSIS
(defmethod scale ((img image) width-factor height-factor
                  &key (interpolation
                        (get-apr-config :default-interpolation)))
;;; ****
  (setf (data img) (imago::scale (data img) width-factor height-factor
                                 :interpolation interpolation))
  img)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* image/copy
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-01
;;; 
;;; DESCRIPTION
;;; This method copies a rectangular region from the src image to the dest
;;; image (cf. imago::copy). Then no width or height are given, the complete
;;; image will be copied.
;;; NB: Both images must be large enough to contain the specified region at the
;;; given positions (cf. imago::copy). 
;;;
;;; ARGUMENTS
;;; - The destination image object.
;;; - The source image object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :dest-x. A number indicating the x-coordinate of the position on the
;;;   destination image. Default = 0
;;; - :dest-y. A number indicating the y-coordinate of the position on the
;;;   destination image. Default = 0
;;; - :src-x. A number indicating the x-coordinate on the src image. Default = 0
;;; - :src-y. A number indicating the y-coordinate on the src image. Default = 0
;;; - :width. The width of the region to cut out from the src-image. When NIL,
;;;   the complete width of the src-image will be used.
;;; - :height. The height of the region to cut out from the src-image. When NIL,
;;;   the complete height of the src-image will be used. 
;;; 
;;; RETURN VALUE
;;; The (modified) dest image object. 
;;;
;;; EXAMPLE
#|
(let ((img1 (make-rgb-image 200 300))
(img2 (make-rgb-image 30 30 :initial-color (make-color 20 90 111))))
(copy img1 img2 :height 10 :width 10
:dest-x 20 :dest-y 25))
;;; =>
IMAGE: width: 200, height: 300
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<RGB-IMAGE (200x300) {7006D1DCB3}>
**********
|#
;;; SYNOPSIS
(defmethod copy ((dest image) (src image)
                 &key
                   (dest-x 0)
                   (dest-y 0)
                   (src-x 0)
                   (src-y 0)
                   width height)
  ;;; ****
  (imago::copy (data dest) (data src)
               :height height
               :width width
               :src-y src-y
               :src-x src-x
               :dest-y dest-y
               :dest-x dest-x)
  dest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod crop ((img image) x y width height)
  ;;; ****
  (setf (data img) (imago::crop (data img) x y width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EXAMPLE:
#|
(let ((img (make-rgb-image 200 200
:initial-color (make-color 129 200 211))))
(rotate img 20))
|#
;;; NB: Rotation is counterclockwise
(defmethod rotate ((img image) degree
                   &key
                     (interpolation (get-apr-config :default-interpolation))
                     (background-color (get-apr-config :default-rgb)))
  ;;; ****
  (setf (data img) (imago::rotate (data img) degree
                                  :interpolation interpolation
                                  :background-color background-color))
  img)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* image/write-png
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-01
;;; 
;;; DESCRIPTION
;;; This method writes the content of the data-slot of an image object to a
;;; png-file. 
;;;
;;; ARGUMENTS
;;; The image object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :outfile. The output filename. Default = "/tmp/image.png"
;;; 
;;; RETURN VALUE
;;; The path to the output file. 
;;;
;;; EXAMPLE
#|
(let ((img1 (make-rgb-image 200 300))
(img2 (make-rgb-image 30 30 :initial-color (make-color 20 90 111))))
(copy img1 img2 :height 10 :width 10
:dest-x 20 :dest-y 25)
(write-png img1))
|#
;;; SYNOPSIS
(defmethod write-png ((img image) &key (outfile "/tmp/image.png"))
  ;;; ****
  (imago-pngio::write-png (data img) outfile)
  outfile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* image/write-jpg
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-01
;;; 
;;; DESCRIPTION
;;; This method writes the content of the data-slot of an image object to a
;;; jpg-file. 
;;;
;;; ARGUMENTS
;;; The image object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :outfile. The output filename. Default = "/tmp/image.jpg"
;;; - :quality. The quality of the output as an integer (0<=q<=100), where 100
;;;   means best quality. Default = 100.
;;; - :overwrite? Overwrite an existing outfile (when T). Default = T
;;; - :warn? Then T, additional warnings are printed.
;;;   Default = (get-apr-config :verbose)
;;; 
;;; RETURN VALUE
;;; The outfile path (string).
;;;
;;; EXAMPLE
#|
(let ((img1 (make-rgb-image 200 300))
(img2 (make-rgb-image 30 30 :initial-color (make-color 20 90 111))))
(copy img1 img2 :height 10 :width 10
:dest-x 20 :dest-y 25)
(write-jpg img1))
|#
;;; SYNOPSIS
(defmethod write-jpg ((img image) &key
                                    (outfile "/tmp/image.jpg")
                                    (quality 100)
                                    (overwrite? t)
                                    (warn? (get-apr-config :verbose)))
  ;;; ****
  (unless (and (integerp quality) (<= 0 quality) (>= 100 quality))
    (error "image::write-jpg: The quality must be an integer [0-100]"))
  (when (and overwrite? (probe-file outfile))
    (delete-file outfile)
    (when warn?
      (warn "image::write-jpg: The outfile ~a exists and will be overwritten."
            outfile)))
  (imago-jpeg-turbo::write-jpg (data img) outfile :quality quality)
  outfile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compose ((dest image) (src image)
                    &key
                      height
                      width
                      (src-y 0)
                      (src-x 0)
                      (dest-y 0)
                      (dest-x 0)
                      complete?
                      (compose-fun #'a-over-b-fun))
  ;;; ****
  (let ((tmp-src src))
    (when (or width height)
      (setf tmp-src (clone src))
      (let ((width (if width
                       width
                       (width tmp-src)))
            (height (if height
                        height
                        (height tmp-src))))
        (setf (data tmp-src)
              (imago::crop (data tmp-src) src-x src-y width height))))
    ;; compose does not support negative dest-coords -> crop
    (when (or (and dest-x (> 0 dest-x))
              (and dest-y (> 0 dest-y)))
      (let* ((width (if width
                        width
                        (width tmp-src)))
             (height (if height
                         height
                         (height tmp-src)))
             (crop-x (if (> 0 dest-x) (abs dest-x) 0))
             (crop-y (if (> 0 dest-y) (abs dest-y) 0))
             (crop-w (- width crop-x))
             (crop-h (- height crop-y)))
        (setf (data tmp-src)
              (imago::crop (data tmp-src) crop-x crop-y crop-w crop-h))
        (when (> 0 dest-x)
          (setf dest-x 0))
        (when (> 0 dest-y)
          (setf dest-y 0))))
    (let ((tmp-src2 tmp-src))
      (when (and complete? (or
                            (< (width tmp-src) (width dest))
                            (< (height tmp-src) (height dest))))
        (setf tmp-src2 (make-rgb-image (width dest) (height dest)
                                       :initial-color
                                       (make-color 255 255 255 0)))
        (compose tmp-src2 tmp-src
                 :dest-x dest-x :dest-y dest-y
                 :src-x src-x :src-y src-y
                 :complete? nil)
        (setf dest-x 0
              dest-y 0
              src-x 0
              src-y 0
              height nil
              width nil))
      (setf (data dest)
            (imago::compose nil (data dest) (data tmp-src2)
                            dest-x dest-y
                            compose-fun))))
  dest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun image-p (thing)
  (typep thing 'image))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF image.lisp
