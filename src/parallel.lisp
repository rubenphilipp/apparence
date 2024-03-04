;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apr/parallel
;;; NAME
;;; parallel
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; PURPOSE
;;; Implementation of some methods related to parallel computing (via
;;; lparallel).
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined
;;;
;;; $$ Last modified:  14:52:34 Mon Mar  4 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* parallel/init-kernel
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-26
;;; 
;;; DESCRIPTION
;;; This function initializes a lparallel kernel with n workers. 
;;;
;;; ARGUMENTS
;;; A number indicating the amount of workers to initialize the kernel with.
;;;
;;; NB: The kernel should always be destroyed before quitting the program (or
;;;     via shutdown-kernel. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :name. A string as a name for the kernel. Default = "apparence kernel"
;;; 
;;; RETURN VALUE
;;; The kernel object. 
;;;
;;; EXAMPLE
#|
(init-kernel (serapeum:count-cpus))
|#
;;; SYNOPSIS
(defun init-kernel (num-workers &key (name "apparence kernel"))
  ;;; ****
  (setf lparallel:*kernel*
        (lparallel:make-kernel num-workers :name name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* parallel/shutdown-kernel
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-26
;;; 
;;; DESCRIPTION
;;; This function destroys/shuts down the lparallel kernels. This is absolutely
;;; necessary (e.g. for garbage collecting etc.). 
;;;
;;; ARGUMENTS
;;; none
;;; 
;;; RETURN VALUE
;;; Either T or NIL (cf. lparallel doc)
;;;
;;; SYNOPSIS
(defun shutdown-kernel ()
  ;;; ****
  (lparallel:end-kernel :wait t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF parallel.lisp
