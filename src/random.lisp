;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apr/random
;;; NAME
;;; random
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; PURPOSE
;;; This package implements a few advanced random functions, mostly relying on
;;; cl-pcg (https://github.com/sjl/cl-pcg).
;;;
;;; [1]: https://docs.stevelosh.com/cl-pcg/usage/
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined
;;;
;;; $$ Last modified:  22:02:32 Mon Mar  4 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* random/*pcg*
;;; NAME
;;; *pcg*
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; DESCRIPTION
;;; A global pcg generator. 
;;; 
(defparameter *pcg* (pcg:make-pcg :seed (get-apr-config :pcg-seed)))
;;; ****


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* random/pcg-random
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; DESCRIPTION
;;; This is a shorthand to the pcg-random function. 
;;;
;;; ARGUMENTS
;;; The random bound.
;;; "If only bound is given, the function acts much like cl:random" [1]
;;; 
;;; OPTIONAL ARGUMENTS
;;; - max.
;;;   "If max is also given, a random number in [bound, max) is chosen." [1]
;;; - inclusive?
;;;   "If inclusive? is also given, a random number in [bound, max] is chosen"
;;;   [1]
;;; 
;;; RETURN VALUE
;;; A random number. 
;;;
;;; EXAMPLE
#|
(pcg-random 100)
|#
;;; SYNOPSIS
(defun pcg-random (bound &optional max inclusive?)
  ;;; ****
  (pcg:pcg-random *pcg* bound max inclusive?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF random.lisp
