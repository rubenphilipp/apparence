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
;;; $$ Last modified:  00:44:07 Sun Apr 21 2024 CEST
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

;; without stopwatch
(with-kernel (:stopwatch? nil)
  (lparallel:pdotimes (i 10)
(print i)))
;; =>
(PROGN
 (INIT-KERNEL 12 :NAME "apparence kernel")
 (PROGN
  (LPARALLEL.COGNATE:PDOTIMES (I 10)
    (PRINT I)))
 (SHUTDOWN-KERNEL))
|#
;;; SYNOPSIS
(defmacro with-kernel ((&key
                          (num-workers (serapeum::count-cpus))
                          (kernel-name "apparence kernel")
                          (stopwatch? t)
                          (sw-start-accessor 'kernel-start)
                          (sw-delta-fun 'kernel-delta)
                          (sw-reset-fun 'kernel-reset))
                       &body body)
  ;;; ****
  (let ((body-form
          (if stopwatch?
              `(with-stopwatch (:start-accessor ,sw-start-accessor
                                :delta-fun ,sw-delta-fun
                                :reset-fun ,sw-reset-fun)
                 ,@body)
              `(progn ,@body))))
    `(progn
       (init-kernel ,num-workers :name ,kernel-name)
       ,body-form
       (shutdown-kernel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** do-frames
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-20
;;; 
;;; DESCRIPTION
;;; This macro (parallely) iterates through the given frames. 
;;;
;;; ARGUMENTS
;;; - var: the accessor for the current frame index.
;;; - end. an integer indicating the last frame of the sequence.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :start. An integer indicating the start frame of the sequence to be
;;;   processed. Default = 0
;;; 
;;; RETURN VALUE
;;; The return value of the body form. 
;;;
;;; EXAMPLE
#|
(do-frames (i 20 :start 10)
  (print i))

;; =>
10 
11 
12 
13 
14 
15 
16 
17 
18 
19 NIL
|#
;;; SYNOPSIS
(defmacro do-frames ((var end &key
                                (start 0))
                     &body body)
  ;;; ****
  ;;; sanity checks
  (unless (and (integerp end) (< 0 end))
    (error "parallel::do-frames: end must be > 0 and of type integer."))
  (unless (and (integerp start) (<= 0 start) (< start end))
    (error "parallel::do-frames: start must be of type integer, > 0 ~
            and < end."))
  `(lparallel:pdotimes (,var ,(- end start))
     (let ((,var (+ ,var ,start)))
       ,@body)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF parallel.lisp
