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
;;; $$ Last modified:  23:23:23 Thu Feb 29 2024 CET
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
  (unless (default-interpolation img)
    (warn "image::update: No default-interpolation is given. Using ~
            imago::*default-interpolation*: ~a"
           imago::*default-interpolation*))
  (unless (typep (data img) 'imago::image)
    (error "image::update: The data-slot of the image object must contain a ~
            imago::image."))
  ;;; set width and height from the imago::image
  (let ((image (data img)))
    (setf (slot-value img 'width) (imago::image-width image)
          (slot-value img 'height) (imago::image-height image))))

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
  (let ((data (imago-pngload::read-pngload path)))
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
;;; These methods reflect some imago methods.

;;; destructive
(defmethod resize ((img image) new-width new-height
                   &key (interpolation
                         (get-apr-config :default-interpolation)))
  ;;; ****
  (setf (data img) (imago::resize (data img) new-width new-height
                                  :interpolation interpolation))
  img)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; destructive
(defmethod scale ((img image) width-factor height-factor
                  &key (interpolation
                        (get-apr-config :default-interpolation)))
  ;;; ****
  (setf (data img) (imago::scale (data img) width-factor height-factor
                                 :interpolation interpolation))
  img)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; non-destructive
;;; returns a new image object
(defmethod copy ((dest image) (src image)
                 &key
                   (dest-x 0)
                   (dest-y 0)
                   (src-x 0)
                   (src-y 0)
                   width height
                   ;; attributes for the new image object
                   id
                   (default-interpolation
                    (get-apr-config :default-interpolation)))
  ;;; ****
  (let ((new (imago::copy (data dest) (data src)
                          :height height
                          :width width
                          :src-y src-y
                          :src-x src-x
                          :dest-y dest-y
                          :dest-x dest-x)))
    (make-image new :id id :default-interpolation default-interpolation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod write-png ((img image) &key (outfile "/tmp/image.png"))
  ;;; ****
  (imago::write-png (data img) outfile))
                          
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF image.lisp
