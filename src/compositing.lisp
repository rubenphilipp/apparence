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
;;; $$ Last modified:  22:25:33 Wed Mar 27 2024 CET
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A generic version of the Porter Duff compositing algebra.
;;; f-a and f-b are the fractional terms (cf. Porter/Duff 1984, 255).

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF compositing.lisp
