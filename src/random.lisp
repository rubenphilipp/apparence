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
;;; $$ Last modified:  00:21:41 Sun May 19 2024 CEST
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
;;; The global pcg generator.
;;; 
(defparameter *pcg* (pcg:make-pcg :seed (get-apr-config :pcg-seed)))
;;; ****


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* random/reset-pcg
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; DESCRIPTION
;;; This function resets the global pcg random state (i.e. it re-initializes the
;;; global pcg-object stored in *pcg*).
;;;
;;; ARGUMENTS
;;; none.
;;;
;;; OPTIONAL ARGUMENTS
;;; - A pcg-seed. Default = (get-apr-config :pcg-seed)
;;; 
;;; RETURN VALUE
;;; The reset pcg object. 
;;; 
;;; SYNOPSIS
(defun reset-pcg (&optional (seed (get-apr-config :pcg-seed)))
  ;;; ****
  (setf *pcg* (pcg:make-pcg :seed seed)))


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
;;; keyword-arguments:
;;; - :max.
;;;   "If max is also given, a random number in [bound, max) is chosen." [1]
;;; - :inclusive
;;;   "If inclusive? is also given, a random number in [bound, max] is chosen"
;;;   [1]
;;; - :reset. When T, the global state is reset to the value defined in :seed.
;;; - :pcg. The pcg-object to be used for the random-generation.
;;;   Default = *pcg*. 
;;; - :seed. A seeding value used when :reset = T.
;;;   Default = (get-apr-config :pcg-seed)
;;; 
;;; RETURN VALUE
;;; A random number. 
;;;
;;; EXAMPLE
#|
(pcg-random 100)                        ; ;
|#
;;; SYNOPSIS
(defun pcg-random (bound &key max inclusive reset
                           (pcg *pcg*)
                           (seed (get-apr-config :pcg-seed)))
;;; ****
  (when reset
    (reset-pcg seed))
  (pcg:pcg-random pcg bound max inclusive))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF random.lisp
