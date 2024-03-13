;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; utilities.lisp
;;;
;;; NAME
;;; utilities
;;;
;;; DESCRIPTION
;;; Utility functions for apparence.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;;
;;; $$ Last modified:  22:58:40 Wed Mar 13 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/trailing-slash
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function ensures that a (path) string ends with a trailing slash.
;;; NB: This function is borrowed from Michael Edward's slippery-chicken.
;;;
;;; ARGUMENTS
;;; A string containing the path to be checked and corrected.
;;; 
;;; RETURN VALUE
;;; The path with a trailing slash.
;;;
;;; SYNOPSIS
(defun trailing-slash (path)
  ;;; ****
  (if (> (length path) 0)
    (if (char= #\/ (elt path (1- (length path))))
        path
        (format nil "~a/" path))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/path-from-same-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function returns the full path to a file relative to the directory of
;;; the current lisp file.
;;; NB: This function is borrowed from Michael Edwards's slippery-chicken.
;;; NB2: This function does not work with files which have been loaded via
;;;      ASDF/quicklisp.
;;;
;;; ARGUMENTS
;;; - A string indicating the filename (or pathname) to the file relative to
;;;   the current lisp file.
;;; 
;;; RETURN VALUE
;;; A string with the full path to the file.
;;; 
;;; SYNOPSIS
(defun path-from-same-dir (file)
   ;;; ****
  (concatenate 'string
               (trailing-slash
                (directory-namestring (truename *load-pathname*)))
               file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/load-from-same-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-26
;;; 
;;; DESCRIPTION
;;; This function loads a lisp file from the file relative to the directory of
;;; the current file (cf. path-from-same-dir).
;;;
;;; ARGUMENTS
;;; - A string indicating the filename (or pathname) to the file relative to
;;;   the current lisp file.
;;; 
;;; RETURN VALUE
;;; The result of the #'load call.
;;;
;;; SYNOPSIS
(defun load-from-same-dir (file)
  ;;; ****
  (load (path-from-same-dir file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/path-from-src-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This function returns a path from the src dir. It is intended to be used
;;; within the main lisp files.
;;; NB: This function requires ASDF. 
;;;
;;; ARGUMENTS
;;; - A string indicating the filename (or pathname) to the file relative to
;;;   the src directory file.
;;; 
;;; RETURN VALUE
;;; A string with the full path to the file.
;;; 
;;; SYNOPSIS
(defun path-from-src-dir (file)
   ;;; ****
  (namestring (asdf::SYSTEM-RELATIVE-PATHNAME
               :apparence
               (concatenate 'string
                            "src/"
                            file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/simple-shell
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Run a shell command from lisp and return the exit code.
;;;
;;; ARGUMENTS
;;; - The shell command (i.e., most likely, path to the binary)
;;; 
;;; OPTIONAL ARGUMENTS:
;;; rest:
;;; - The arguments to the shell program.
;;; 
;;; RETURN VALUE
;;; The the exit-code of the process.
;;;
;;; SYNOPSIS
(defun simple-shell (command &rest arguments)
  ;;; ****
  (cl-user::process-exit-code
   (cl-user::run-program command arguments :output *standard-output*
                                           :wait t :input nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/shell
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Runs a shell program and return the full result as a string or throws an
;;; error when the call to the program fails.
;;;
;;; ARGUMENTS
;;; - The command (e.g. a path to a binary).
;;; 
;;; OPTIONAL ARGUMENTS
;;; rest:
;;; - The arguments to the shell program. 
;;; 
;;; RETURN VALUE
;;; The result of the shell program call as a string.
;;;
;;; EXAMPLE
#|
(shell "git" "-v")
;; => "git version 2.40.1"
|#
;;; SYNOPSIS
(defun shell (command &rest arguments)
  ;;; ****
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (cons command arguments)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (unless (zerop exit-code)
      (error "utilities::shell: The call to ~a failed. Error output: ~a ~%"
             command error-output))
    output))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/system-open-file
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; DESCRIPTION
;;; Open a file using the system's default program. 
;;;
;;; ARGUMENTS
;;; A path to the file to open. 
;;; 
;;; RETURN VALUE
;;; A boolean indicating whether the operation succeeded. 
;;;
;;; SYNOPSIS
(defun system-open-file (file)
  ;;; ****
  (shell "/usr/bin/open" file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/aspect-ratio
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; DESCRIPTION
;;; Returns the aspect-ratio (w/h) of a given image. 
;;;
;;; ARGUMENTS
;;; An image object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; none
;;; 
;;; RETURN VALUE
;;; The aspect ratio. 
;;;
;;; SYNOPSIS
(defun aspect-ratio (image)
  ;;; ****
  (/ (width image)
     (height image)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/degrees->radians
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; This function converts a value in degrees to its radian counterpart. 
;;;
;;; ARGUMENTS
;;; The value (degrees) to be converted. 
;;; 
;;; RETURN VALUE
;;; The value in radians. 
;;;
;;; EXAMPLE
#|
(degrees->radians -30)
;; => -0.5235987755982988d0
|#
;;; SYNOPSIS
(defun degrees->radians (degrees)
  ;;; ****
  (* pi (/ degrees 180)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/radians->degrees
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; This function converts radians to degrees.
;;;
;;; ARGUMENTS
;;; The radian value to convert. 
;;; 
;;; RETURN VALUE
;;; The value in degrees. 
;;;
;;; EXAMPLE
#|
(radians->degrees (degrees->radians -90))
;; => -90.0
|#
;;; SYNOPSIS
(defun radians->degrees (rad)
  ;;; ****
  (* rad (/ 180 pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/fround-to-digits
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; This function (f)rounds a floating point number to the given amount of
;;; digits.
;;;
;;; ARGUMENTS
;;; - The floating point number.
;;;
;;; OPTIONAL ARGUMENTS.
;;; - An integer indicating the number of digits to be frounded to. Default = 0
;;; 
;;; RETURN VALUE
;;; The rounded float. 
;;;
;;; EXAMPLE
#|
(fround-to-digits 3.061616997868383 3)
;; => 3.062
|#
;;; SYNOPSIS
(defun fround-to-digits (v &optional (digits 0))
  ;;; ****
  (declare (type float v)
           (type (integer 0) digits))
  (let ((10^-digits (expt 10 (- digits))))
    (* (fround v 10^-digits)
       10^-digits)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/rgb-p
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; Tests if a given value is of type rgb-list. 
;;;
;;; ARGUMENTS
;;; The object to test. 
;;; 
;;; RETURN VALUE
;;; Either T or NIL. 
;;;
;;; EXAMPLE
#|
(rgb-p '(255 125 254))
;; => T
|#
;;; SYNOPSIS
(defun rgb-p (thing)
  ;;; ****
  (and (listp thing) (= 3 (length thing))
       (every #'integerp thing)
       (every #'(lambda (x)
                  (and (<= 0 x)
                       (>= 255 x)))
              thing)))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/rgba-p
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-28
;;; 
;;; DESCRIPTION
;;; Tests if a given value is of type rgba-list. 
;;;
;;; ARGUMENTS
;;; The object to test. 
;;; 
;;; RETURN VALUE
;;; Either T or NIL. 
;;;
;;; EXAMPLE
#|
(rgba-p '(255 125 254))
;; => NIL
(rgba-p '(125 222 2 230))
;; => T
|#
;;; SYNOPSIS
(defun rgba-p (thing)
  ;;; ****
  (and (listp thing) (= 4 (length thing))
       (every #'integerp thing)
       (every #'(lambda (x)
                  (and (<= 0 x)
                       (>= 255 x)))
              thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns a list of xy-lists from an envelope.
#|
(env->xy-list '(0 20 20 -5 60 80 100 0))
;; => ((0 20) (20 -5) (60 80) (100 0))
|#
(defun env->xy-list (env)
  (unless (evenp (length env))
    (error "utilities::env->xy-list: The envelope is malformed."))
  (loop for x in env by #'cddr and y in (rest env) by #'cddr
        collect (list x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns an envelope from a list of xy-lists.
#|
(xy-list->env '((0 1) (20 -5) (100 -80)))
;; => (0 1 20 -5 100 -80)
|#
(defun xy-list->env (xy-list)
  (unless (every #'(lambda (x)
                     (and (listp x)
                          (eq 2 (length x))))
                 xy-list)
    (error "utilities::xy-list->env: The xy-list is malformed."))
  (loop for val in xy-list
        append (list (first val) (second val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/plot-envelope
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-29
;;; 
;;; DESCRIPTION
;;; This function plots an envelope via vgplot (gnuplot). 
;;;
;;; ARGUMENTS
;;; The envelope list to plot. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :label. A string used as a label for the plot. Could also contain
;;;   formatting information (cf. vgplot API).
;;; - :new-plot?. When T, a new plot will be created. 
;;; 
;;; RETURN VALUE
;;; none
;;;
;;; EXAMPLE
#|
(plot-envelope '(0 -100 30 0 60 40 70 40 85 0 100 10) :label "env1")
|#
;;; SYNOPSIS
(defun plot-envelope (env &key label new-plot?)
  ;;; ****
  (let ((gnuplot-data (env->xy-list env)))
    (when new-plot? (vgplot::new-plot))
    (vgplot::plot (mapcar #'first gnuplot-data)
                  (mapcar #'second gnuplot-data)
                  label)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/plot-envelopes
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-29
;;; 
;;; DESCRIPTION
;;; This function plots multiple envelopes via vgplot (gnuplot). The envelopes
;;; will be given as a list of envelope-lists. Additionally, a label can be
;;; added to each envelope. The :labels have to be a list with vgplot
;;; label-strings. 
;;;
;;; ARGUMENTS
;;; The list of envelope lists to plot. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :labels. A list with label-strings (cf. vplot-API).
;;; - :new-plot?. When T, a new plot will be created.
;;; - :x-axis-label. A label string for the x-axis.
;;; - :y-axis-label. A label string for the y-axis. 
;;; 
;;; RETURN VALUE
;;; The result of the vgplot:plot process. 
;;;
;;; EXAMPLE
#|
(plot-envelopes '((0 -5 20 20 100 80)
                  (0 -2.5 30 5 80 90 100 0)
                  (0 -4 40 2 100 20.5))
                :labels '("env1" "env2")
                :x-axis-label "time (s)"
                :y-axis-label "intensity")

|#
;;; SYNOPSIS
(defun plot-envelopes (envelopes &key
                                   labels
                                   new-plot?
                                   x-axis-label
                                   y-axis-label)
  ;;; ****
  (unless (every #'listp envelopes)
    (error "utilities::plot-envelopes: The envelopes must be a list of lists."))
  (when (> (length labels) (length envelopes))
    (warn "utilities::plot-envelopes: More labels than envelopes are given. ~
           Remaining labels will be ignored."))
  (let ((plot-data (loop for i from 0 to (1- (length envelopes))
                         for env = (nth i envelopes)
                         for label = (nth i labels)
                         for env-data = (env->xy-list env)
                         append
                         (list (mapcar #'first env-data)
                               (mapcar #'second env-data)
                               (if label label "")))))
    (when new-plot? (vgplot::new-plot))
    (apply #'vgplot::plot plot-data)
    (set-plot-axis-label :x-label x-axis-label
                         :y-label y-axis-label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sets the plot-labels
#|
(set-plot-axis-label :x-label "secs")
|#
(defun set-plot-axis-label (&key
                              x-label
                              y-label)
  (when x-label (vgplot::xlabel x-label))
  (when y-label (vgplot::ylabel y-label)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(color->list 4294181120)
;; => (244 1 0 255)
|#
(defun color->list (color)
  ;;; ****
  (let ((r (imago::color-red color))
        (g (imago::color-green color))
        (b (imago::color-blue color))
        (a (imago::color-alpha color)))
    (list r g b a)))

#|
(list->color '(244 1 2))
;; => 49545472
|#
(defun list->color (clist)
  ;;; ****
  (unless (or (rgb-p clist) (rgba-p clist))
    (error "utilities::list->color: The list is not of type rgb(a)-list"))
  (apply #'imago::make-color clist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/get-random-uuid
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; DESCRIPTION
;;; This method returns a UUIV v4 (random UUID), generated via frugal-uuid. 
;;;
;;; ARGUMENTS
;;; none.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :string. A boolean indicating whether a string (t) or fuuid-object schould
;;;   be returned. Default = T
;;; 
;;; RETURN VALUE
;;; The UUID as a string. 
;;;
;;; EXAMPLE
#|
(get-random-uuid)
;; => "31db7363-f393-4fbc-ad26-10ffb6339640"
|#
;;; SYNOPSIS
(defun get-random-uuid (&key (string t))
  ;;; ****
  (let ((uuid (fuuid:make-v4)))
    (if string
        (fuuid:to-string uuid)
        uuid)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** utilities/with-stopwatch
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-13
;;; 
;;; DESCRIPTION
;;; This macro counts the time (in seconds) from the start of running the forms
;;; in the body. The start time (i.e. the universal timestamp) can be retrieved
;;; via the symbol :start-accessor (cf. arguments).
;;; There are some local functions which allow to retrieve further information
;;; or interact with the state of the "stopwatch":
;;; - delta-fun (&optional print?)
;;;   This local function returns the difference between start-time and current
;;;   time (i.e. the time spent for performing the forms).
;;;   When print? = T, the time will be printed via (print).
;;; - reset-fun
;;;   This local function resets the start-time to the current timestamp.
;;;
;;; ARGUMENTS
;;; none.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :start-accessor. The accessor (symbol) to the start time.
;;;   Default = 'sw-start
;;; - :delta-fun. The function-name (symbol) for the delta-function (see above).
;;;   Default = 'sw-delta
;;; - :reset-fun. The function-name (symbol) for the reset-function (see above).
;;;   Default = 'sw-reset
;;;
;;; EXAMPLE
#|
(with-stopwatch ()
  (sleep 1)
  (sw-delta t)
  (sleep 1)
  (print sw-start)
  (sw-delta t)
  (sw-reset)
  (sleep 1)
  (sw-delta))
;; =>
1 
3919355895 
2 
1
|#
;;; SYNOPSIS
(defmacro with-stopwatch ((&key
                             (start-accessor 'sw-start)
                             (delta-fun 'sw-delta)
                             (reset-fun 'sw-reset))
                          &body body)
  ;;; ****
  `(let ((,start-accessor (get-universal-time)))
     (flet ((,delta-fun (&optional (print? nil))
              (let ((delta (- (get-universal-time) ,start-accessor)))
                (if print?
                    (print delta)
                    delta)))
            (,reset-fun ()
              (setf ,start-accessor (get-universal-time))))
       ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lisp
