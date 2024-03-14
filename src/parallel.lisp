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
;;; $$ Last modified:  22:18:35 Thu Mar 14 2024 CET
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
;;; ****** parallel/with-kernel
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-14
;;; 
;;; DESCRIPTION
;;; This macro starts a lparallel kernel around the body and optionally
;;; instantiates a stopwatch for measuring the kernel runtime
;;; (cf. with-stopwatch).
;;;
;;; ARGUMENTS
;;; none.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :num-workers. A number indicating the amount of workers to initialize the
;;;   kernel with. Default = (serapeum:count-cpus) -> i.e. the number of
;;;   available CPU cores. 
;;; - :kernel-name. A string as a name for the kernel.
;;;   Default = "apparence kernel"
;;; - :stopwatch?. A boolean indicating whether to measure the running time of
;;;   the kernel (when T; via with-stopwatch). Default = t
;;; - :sw-start-accessor. The accessor (symbol) to the stopwatch start time.
;;;   Default = kernel-start
;;; - :sw-delta-fun. The function-name (symbol) for the delta-function
;;;   (cf. with-stopwatch). Default = 'kernel-delta
;;; - :sw-reset-fun. The function-name (symbol) for the reset-function
;;;   (cf. with-stopwatch). Default = 'kernel-reset
;;;
;;; EXAMPLE
#|
(with-kernel ()
  (lparallel:pdotimes (i 10)
    (print i)))
;; =>
(PROGN
 (INIT-KERNEL 12 :NAME "apparence kernel")
 (WITH-STOPWATCH (:START-ACCESSOR KERNEL-START :DELTA-FUN KERNEL-DELTA
                  :RESET-FUN KERNEL-RESET)
   (LPARALLEL.COGNATE:PDOTIMES (I 10)
     (PRINT I)))
 (SHUTDOWN-KERNEL))
|#
;;; SYNOPSIS
(defmacro with-kernel ((&key
                        (num-workers (serapeum:count-cpus))
                        (kernel-name "apparence kernel")
                        (stopwatch? t)
                        (sw-start-accessor 'kernel-start)
                        (sw-delta-fun 'kernel-delta)
                        (sw-reset-fun 'kernel-reset))
                       &body body)
  ;;; ****
  `(progn
     (init-kernel ,num-workers :name ,kernel-name)
     (with-stopwatch (:start-accessor ,sw-start-accessor
                      :delta-fun ,sw-delta-fun
                      :reset-fun ,sw-reset-fun)
       ,@body)
     (shutdown-kernel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF parallel.lisp
