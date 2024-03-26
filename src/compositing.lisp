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
;;; $$ Last modified:  22:26:02 Tue Mar 26 2024 CET
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
;;; ****f* compositing/a-over-b-op
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-26
;;; 
;;; DESCRIPTION
;;; This is an implementation of the Porter/Duff A Over B algorithm.
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
  (unless (and (rgba-list-p a) (rgba-list-p b))
    (error "compositing::a-over-b-op: Both parameters need to be of type ~
            rgba-list."))
  (labels ((get-alpha (alpha-a alpha-b)
               (+ alpha-a
                  (* (- 1 alpha-a) alpha-b)))
           (get-color (alpha-a alpha-b alpha-c a b)
             (let ((tmp-result (+ (* alpha-a a)
                                  (* b alpha-b (- 1 alpha-a)))))
               ;; prevent floating point division by zero
               (if (< 0 alpha-c)
                   (/ tmp-result alpha-c)
                   1.0))))
    (let ((alpha-c (get-alpha (fourth a) (fourth b))))
      (list (get-color (fourth a) (fourth b) alpha-c (first a) (first b))
            (get-color (fourth a) (fourth b) alpha-c (second a) (second b))
            (get-color (fourth a) (fourth b) alpha-c (third a) (third b))
            alpha-c))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF compositing.lisp
