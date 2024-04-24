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
;;; $$ Last modified:  16:54:43 Wed Apr 24 2024 CEST
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
;;; EOF imago.lisp
