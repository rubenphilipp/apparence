;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apr/compositing
;;; NAME
;;; compositing
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-26
;;; 
;;; PURPOSE
;;; This module implements image compositing algorithms. 
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined. 
;;;
;;; $$ Last modified:  17:09:45 Wed Apr 24 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* compositing/rgba-list-p
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-26
;;; 
;;; DESCRIPTION
;;; Tests if a given item is a rgba-list.  An rgba-list is a four-element list
;;; with each element e being a floating point value 0 <= e <= 1. The order is:
;;; (r b g a).
;;;
;;; ARGUMENTS
;;; The thing to be tested. 
;;; 
;;; RETURN VALUE
;;; Either T or NIL. 
;;;
;;; EXAMPLE
#|
(rgba-list-p '(1 0 1 .2)) => T
|#
;;; SYNOPSIS
(defun rgba-list-p (thing)
  ;;; ****
  (and (listp thing)
       (eq (length thing) 4)
       (every #'(lambda (x)
                  (and (numberp x)
                       (<= 0 x)
                       (>= 1 x)))
              thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** compositing/porter-duff-comp
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;; 
;;; DESCRIPTION
;;; This macro is a general form of the Porter/Duff compositing operation (cf.
;;; Porter/Duff 1984). It expands to a lambda function that takes two rgba-lists
;;; as its arguments and returns a new rgba-list of the computed color.
;;; 
;;; The two optional arguments to the macro are the fractional terms as
;;; described by Porter and Duff (1984, 255). They can changed according to
;;; the operation to implement. Cf. a-over-b-op for an example.
;;;
;;; Literature:
;;; - Porter, Thomas, und Tom Duff. 1984. „Compositing Digital Images“. In
;;;   Proceedings of the 11th Annual Conference on Computer Graphics and
;;;   Interactive Techniques,
;;;   253–59. ACM. https://doi.org/10.1145/800031.808606.
;;;
;;; ARGUMENTS
;;; none to the macro.
;;; Arguments to the lambda function resulting during macroexpansion:
;;; Two rgba-lists (cf. rgba-list-p) for color a and b. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; The two fractional terms, f-a and f-b (see above).
;;; 
;;; RETURN VALUE
;;; A new rgba-color as a list. 
;;;
;;; EXAMPLE
#|
(let ((a '(.3 .4 .6 .2))
      (b '(.4 .1 .2 .9)))
  ;; this is the A Over B operation:
  (funcall (porter-duff-comp 1 (- 1 a-a)) a b))
;; => (0.37826088 0.16521741 0.28695652 0.91999996)
;;; SYNOPSIS
|#
(defmacro porter-duff-comp (&optional (f-a 1) (f-b '(- 1 a-a)))
  ;;; ****
  `(lambda (a b)
     (unless (and (rgba-list-p a) (rgba-list-p b))
       (error "compositing::porter-duff-comp: Both parameters need to be of ~
               type rgba-list."))
     (flet ((get-color (a-a a-b c-a c-b)
              (+ (* a-a ,f-a c-a)
                         (* a-b ,f-b c-b))))
       (let* ((a-a (fourth a))
              (a-b (fourth b))
              (a-c (+ (* a-a ,f-a)
                     (* a-b ,f-b))))
         (list (get-color (fourth a) (fourth b) (first a) (first b))
               (get-color (fourth a) (fourth b) (second a) (second b))
               (get-color (fourth a) (fourth b) (third a) (third b))
               a-c)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* compositing/a-over-b-op
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-26
;;; 
;;; DESCRIPTION
;;; This is an implementation of the Porter/Duff A Over B compositing operator.
;;;
;;; Literature:
;;; - Porter, Thomas, und Tom Duff. 1984. „Compositing Digital Images“. In
;;;   Proceedings of the 11th Annual Conference on Computer Graphics and
;;;   Interactive Techniques,
;;;   253–59. ACM. https://doi.org/10.1145/800031.808606.
;;; - https://ssp.impulsetrain.com/porterduff.html
;;;
;;; ARGUMENTS
;;; Two rgba-lists (cf. rgba-list-p) for color a and b. 
;;; 
;;; RETURN VALUE
;;; A new rgba-list for the new pixel color. 
;;;
;;; EXAMPLE
#|
(let ((a '(.3 .4 .6 .2))
      (b '(.4 .1 .2 .9)))
  (a-over-b-op a b))
;; => (0.37826088 0.16521741 0.28695652 0.91999996)
|#
;;; SYNOPSIS
(defun a-over-b-op (a b)
  ;;; ****
  (funcall (porter-duff-comp 1 (- 1 a-a)) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-in-b-op (a b)
  (funcall (porter-duff-comp a-b 0) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-out-b-op (a b)
  (funcall (porter-duff-comp (- 1 a-b) 0) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-xor-b-op (a b)
  (funcall (porter-duff-comp (- 1 a-b) (- 1 a-a)) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a-atop-b-op (a b)
  (funcall (porter-duff-comp a-b (- 1 a-a)) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compositing functions
;;; To be used e.g. with imago::compose

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** compositing/compose-op
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
;;; The function a-over-b-fun illustrates a use case of this macro. 
;;;
;;; Note: In order to pass the resulting value back to imago::compose, it is
;;;       necessary to apply the function rgba-list->color to the resulting
;;;       value (if it is an rgba-list); cf. a-over-b-fun. 
;;;
;;; ARGUMENTS
;;; - The first imago-color (i.e. the dest-, in Porter/Duff-terms the B-color).
;;; - The second imago-color (i.e. the src-, in Porter/Duff-terms the A-color).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :a-accessor. The accessor for the a color in the body (this is the value
;;;   of color2, as color1 is the dest and color2 is the src). Default = a.
;;; - :b-accessor. The accessor for the b color in the body (this is the value
;;;   of color1, as color1 is the dest and color2 is the src). Default = b.
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
;;; ****f* compositing/a-over-b-fun
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;; 
;;; DESCRIPTION
;;; This is the compositing function A Over B to be used for example in
;;; imago::compose. 
;;;
;;; ARGUMENTS
;;; Two imago-colors. 
;;; 
;;; RETURN VALUE
;;; A new imago-color. 
;;;
;;; SYNOPSIS
(defun a-over-b-fun (color1 color2)
  ;;; ****
  (compose-op (color1 color2)
    (rgba-list->color (a-over-b-op a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* compositing/a-in-b-fun
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;; 
;;; DESCRIPTION
;;; This is the compositing function A In B to be used for example in
;;; imago::compose. 
;;;
;;; ARGUMENTS
;;; Two imago-colors. 
;;; 
;;; RETURN VALUE
;;; A new imago-color. 
;;;
;;; SYNOPSIS
(defun a-in-b-fun (color1 color2)
  ;;; ****
  (compose-op (color1 color2)
    (rgba-list->color (a-in-b-op a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* compositing/a-out-b-fun
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;; 
;;; DESCRIPTION
;;; This is the compositing function A Out B to be used for example in
;;; imago::compose. 
;;;
;;; ARGUMENTS
;;; Two imago-colors. 
;;; 
;;; RETURN VALUE
;;; A new imago-color. 
;;;
;;; SYNOPSIS
(defun a-out-b-fun (color1 color2)
  ;;; ****
  (compose-op (color1 color2)
    (rgba-list->color (a-out-b-op a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* compositing/a-xor-b-fun
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;; 
;;; DESCRIPTION
;;; This is the compositing function A Xor B to be used for example in
;;; imago::compose. 
;;;
;;; ARGUMENTS
;;; Two imago-colors. 
;;; 
;;; RETURN VALUE
;;; A new imago-color. 
;;;
;;; SYNOPSIS
(defun a-xor-b-fun (color1 color2)
  ;;; ****
  (compose-op (color1 color2)
    (rgba-list->color (a-xor-b-op a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* compositing/a-atop-b-fun
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-27
;;; 
;;; DESCRIPTION
;;; This is the compositing function A Atop B to be used for example in
;;; imago::compose. 
;;;
;;; ARGUMENTS
;;; Two imago-colors. 
;;; 
;;; RETURN VALUE
;;; A new imago-color. 
;;;
;;; SYNOPSIS
(defun a-atop-b-fun (color1 color2)
  ;;; ****
  (compose-op (color1 color2)
    (rgba-list->color (a-atop-b-op a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* compositing/difference-fun
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-24
;;; 
;;; DESCRIPTION
;;; This is the compositing function for a difference value of colors A and B.
;;; The opacity is determined by the alpha value of B. 
;;;
;;; ARGUMENTS
;;; Two imago-colors. 
;;; 
;;; RETURN VALUE
;;; A new imago-color. 
;;;
;;; SYNOPSIS
(defun difference-fun (color1 color2)
  ;;; ****
  (compose-op (color1 color2)
    (rgba-list->color
     (list (abs (- (first a) (first b)))
           (abs (- (second a) (second b)))
           (abs (- (third a) (third b)))
           (abs (fourth b))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* compositing/multiply-fun
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-24
;;; 
;;; DESCRIPTION
;;; This function multiplies the colors of the two images with each other. 
;;;
;;; ARGUMENTS
;;; Two imago-colors. 
;;; 
;;; RETURN VALUE
;;; The resulting color. 
;;;
;;; SYNOPSIS
(defun multiply-fun (color1 color2)
  ;;; ****
  (compose-op (color1 color2)
    (rgba-list->color
     (if (zerop (fourth a))
         b
         (list (min (first a) (first b) (* (first a) (first b)))
           (min (second a) (second b) (* (second a) (second b)))
           (min (third a) (third b) (* (third a) (third b)))
           (max (fourth a) (fourth b) (* (fourth a) (fourth b))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* compositing/apr-default-compose-fun
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
  (apr-default-compose-fun c1 c2))
;; => 4283640847
|#
;;; SYNOPSIS
(defun apr-default-compose-fun (color1 color2)
  ;;; ****
  (funcall #'a-over-b-fun color1 color2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF compositing.lisp
