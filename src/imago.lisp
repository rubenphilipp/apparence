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
;;; $$ Last modified:  00:03:12 Wed Mar 27 2024 CET
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
    (error "imago::rgba-list->color: The list is not a rgba-list."))
  (let ((vals (mapcar #'float->8bit list)))
    (imago::make-color (first vals)
                       (second vals)
                       (third vals)
                       (fourth vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to be used with imago::compose

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** imago/compose-op
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-26
;;; 
;;; DESCRIPTION
;;; This macro is a shortcut to apply a compositing operator (e.g. a-over-b-op)
;;; to two imago-colors, as demanded by imago::compose. It converts both colors
;;; to rgba-lists (cf. rgba-list-p) to be used in the actual compose-operator
;;; and provides them in the body of the macro (accessible via the accessors as
;;; defined in the key arguments).
;;;
;;; Note: In order to pass the resulting value back to imago::compose, it is
;;;       necessary to apply the function rgba-list->color to the resulting
;;;       value (if it is an rgba-list); cf. apr-default-compose-op. 
;;;
;;; ARGUMENTS
;;; - The first imago-color (i.e. the dest-, in Porter/Duff-terms the B-color).
;;; - The second imago-color (i.e. the src-, in Porter/Duff-terms the A-color).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :a-accessor. The accessor for the a color in the body (this is the value
;;;   of color2, as color1 is the dest and color2 is the src). Default = a.
;;; - :b-accessor. The accessor for the b color in the body (this is the value
;;;   of color2, as color1 is the dest and color2 is the src). Default = b.
;;;
;;; EXAMPLE
#|
(let ((c1 (imago::make-color 23 45 19 22))
      (c2 (imago::make-color 83 15 44 255)))
  (compose-op (c1 c2 :a-accessor a :b-accessor b)
    (print a)
    (print b)))
;; => (in the REPL)
;; (0.3254902 0.17254902 0.058823533 1.0) 
;; (0.09019608 0.07450981 0.1764706 0.08627451) 
|#
;;; SYNOPSIS
(defmacro compose-op ((color1 color2
                       &key
                         ;; Color a and be are the colors in the Porter/Duff
                         ;; sense, not in imago's logic.
                         ;; The colors are rgba-lists. 
                         (a-accessor 'a)
                         (b-accessor 'b))
                      &body body)
  ;;; ****
  `(multiple-value-bind (a1 r1 g1 b1) (imago::color-argb ,color1)
     (multiple-value-bind (a2 r2 g2 b2) (imago::color-argb ,color2)
       ;; in imago::compose, color1 is the dest and color2 the src, thus
       ;; values are changed (i.e. 1 = b, 2 = a)
       (let ((,a-accessor (mapcar #'8bit->float (list r2 g2 b2 a2)))
             (,b-accessor (mapcar #'8bit->float (list r1 g1 b1 a1))))
         ,@body))))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* imago/apr-default-compose-op
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-26
;;; 
;;; DESCRIPTION
;;; This is the default compose operator, to be used, e.g. via imago::compose.
;;; By default, this uses the Porter/Duff A-Over-B algorithm for generating the
;;; the resulting color.
;;;
;;; This function is meant to be used with the imago::compose function.
;;;
;;; ARGUMENTS
;;; Two imago-colors, where color1 is the dest (B), and color2 is the src (A)
;;; color (cf. compose-op).
;;; 
;;; RETURN VALUE
;;; An imago-color (integer). 
;;;
;;; EXAMPLE
#|
(let ((c1 (imago::make-color 23 45 19 22))
      (c2 (imago::make-color 83 15 44 255)))
  (apr-default-compose-op c1 c2))
;; => 4283640847
|#
;;; SYNOPSIS
(defun apr-default-compose-op (color1 color2)
  ;;; ****
  (compose-op (color1 color2 :a-accessor a :b-accessor b)
    (rgba-list->color (a-over-b-op a b))))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF imago.lisp
