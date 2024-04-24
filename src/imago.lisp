;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apr/imago
;;; NAME
;;; imago
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-26
;;; 
;;; PURPOSE
;;; Implementation of functionality related to the imago library. 
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined. 
;;;
;;; $$ Last modified:  23:19:28 Wed Apr 24 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* imago/rgba-list->color
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-26
;;; 
;;; DESCRIPTION
;;; This function converts a rgba-list (cf. rgba-list-p) to an imago::color.
;;;
;;; ARGUMENTS
;;; The rgba-list to convert. 
;;; 
;;; RETURN VALUE
;;; An imago::color (which is not an object but an integer). 
;;;
;;; EXAMPLE
#|
(rgba-list->color '(.3 .4 .1 1)) ;; => 4283196953
|#
;;; SYNOPSIS
(defun rgba-list->color (list)
  ;;; ****
  (unless (rgba-list-p list)
    (error "imago::rgba-list->color: The list ~a is not a rgba-list."
           list))
  (let ((vals (mapcar #'float->8bit list)))
    (imago::make-color (first vals)
                       (second vals)
                       (third vals)
                       (fourth vals))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* imago/color->rgba-list
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-24
;;; 
;;; DESCRIPTION
;;; This function converts an imago-color to an rgba-list. 
;;;
;;; ARGUMENTS
;;; The imago-color to convert. 
;;; 
;;; RETURN VALUE
;;; A rgba-list. 
;;;
;;; EXAMPLE
#|
(color->rgba-list (make-color 23 148 23 255))
|#
;;; SYNOPSIS
(defun color->rgba-list (color)
  ;;; ****
  (multiple-value-bind (a r g b)
      (imago::color-argb color)
    (mapcar #'8bit->float (list r g b a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** imago/do-imago-pixels
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-24
;;; 
;;; DESCRIPTION
;;; This macro maps over the pixel values in an apparence object containing
;;; pixel data (e.g. an imago-image) an binds the color, x and y value to the
;;; respective value of the pixel (cf. imago:do-image-pixels). 
;;;
;;; ARGUMENTS
;;; - An apparence object.
;;; - A symbol to bind the imago:color to.
;;; - A symbol to bind the x-coordinate to.
;;; - A symbol to bind the y-coordinate to.
;;; 
;;; BODY
;;; Any form. 
;;; 
;;; SYNOPSIS
(defmacro do-imago-pixels ((image color x y)
                           &body body)
  ;;; ****
  `(let ((img (cond ((or (image-p ,image)
                         (projection-p ,image))
                     (data ,image))
                    ((or (projection-surface-p ,image)
                         (canvas-p ,image))
                     (data (data ,image)))
                    (t (error "imago::do-imago-pixels: No method for an ~
                               object of type ~a." (type-of ,image))))))
     (imago:do-image-pixels (img ,color ,x ,y)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** imago/with-imago-color
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-24
;;; 
;;; DESCRIPTION
;;; This macro binds the values of an imago-color to the given symbols.
;;; The example illustrates a possible use case. 
;;;
;;; ARGUMENTS
;;; - The color of an imago-image (e.g. retrieved via do-imago-pixels).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :a, :r, :g, and :b. The ARGB accessor symbols.
;;; 
;;; BODY
;;; Any form. 
;;;
;;; EXAMPLE
#|
(let ((cv (make-canvas 200 300 :color '(200 128 21 50))))
  (do-imago-pixels (cv color x y)
    (with-imago-color (color :a alpha :r red :g green :b blue)
      (setf color (make-color blue (+ red 10) red 90))))
  (system-open-file (write-png cv)))
|#
;;; SYNOPSIS
(defmacro with-imago-color ((color &key (a 'a) (r 'r) (g 'g) (b 'b))
                            &body body)
  ;;; ****
  `(multiple-value-bind (,a ,r ,g ,b)
       (imago::color-argb ,color)
     ,@body))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF imago.lisp
