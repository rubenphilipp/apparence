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
;;; $$ Last modified:  23:32:55 Wed Oct 16 2024 CEST
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
;;; ****f* utilities/files-from-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-24
;;; 
;;; DESCRIPTION
;;; Returns a list of files contained in the given directory. 
;;;
;;; ARGUMENTS
;;; The directory containing the desired files. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :pattern. A string being a search pattern. If nil, all files will be
;;;   listed. Default = NIL.
;;; - :hidden? A boolean indicating whether hidden files (those starting with
;;;   a dot) should be included in the list (when T). Default = NIL. 
;;; 
;;; RETURN VALUE
;;; A list with the file pathnames in the directory. 
;;;
;;; EXAMPLE
#|
(files-from-dir "~/Desktop/graphic-notation/" :pattern "*.png")
;; =>
(#P"/Users/someuser/Desktop/graphic-notation/1_imp.png"
 #P"/Users/someuser/Desktop/graphic-notation/2_env.png"
 #P"/Users/someuser/Desktop/graphic-notation/3_evnt.png")
|#
;;; SYNOPSIS
(defun files-from-dir (dir &key
                             pattern
                             hidden?)
  ;;; ****
  (let* ((dir (trailing-slash dir))
         (files (if pattern
                    (uiop:directory-files dir pattern)
                    (uiop:directory-files dir))))
    (unless hidden?
      (setf files (remove-if #'uiop:hidden-pathname-p files)))
    files))


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
  (let ((fl (uiop:native-namestring file)))
    (shell "/usr/bin/open" fl)))


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
;;; ****f* utilities/sort-env
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-10-16
;;; 
;;; DESCRIPTION
;;; This function sorts an envelope.
;;;
;;; ARGUMENTS
;;; The envelope list to sort. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :predicate. A function used to sort the list. Default = #'<
;;; 
;;; RETURN VALUE
;;; The sorted envelope list. 
;;;
;;; EXAMPLE
#|
(sort-env '(4 .2 2 6 80 2 43 4 99 4 1 10 100 0))
;; => (1 10 2 6 4 0.2 43 4 80 2 99 4 100 0)
|#
;;; SYNOPSIS
(defun sort-env (env &key (predicate #'<))
;;; ****
  (unless (functionp predicate)
    (error "utilities::sort-env: predicate must be of type function."))
  (xy-list->env
   (env->xy-list env :sort predicate)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns a list of xy-lists from an envelope.
#|
(env->xy-list '(0 20 20 -5 60 80 100 0)) 
;; => ((0 20) (20 -5) (60 80) (100 0)) 
|#
(defun env->xy-list (env &key (sort nil))
  (unless (evenp (length env))
    (error "utilities::env->xy-list: The envelope is malformed."))
  (unless (or (functionp sort) (null sort))
    (error "utilities::env->xy-list: sort must be of type function."))
  (let ((res (loop for x in env by #'cddr and y in (rest env) by #'cddr
                   collect (list x y))))
    (if sort
        (sort res sort :key #'car)
        res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RP  Thu Apr 25 22:48:09 2024

(defun xy-list-p (thing)
  (every #'(lambda (x)
             (and (listp x)
                  (eq 2 (length x))))
         thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns an envelope from a list of xy-lists.
#|
(xy-list->env '((0 1) (20 -5) (100 -80)))
;; => (0 1 20 -5 100 -80)
|#
(defun xy-list->env (xy-list)
  (unless (xy-list-p xy-list)
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
                  (if label label ""))))


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
;;; - :print-total. Print the delta time after the body (when T)?
;;;   Default = NIL
;;; - :start-accessor. The accessor (symbol) to the start time.
;;;   Default = 'sw-start
;;; - :delta-fun. The function-name (symbol) for the delta-function (see above).
;;;   Default = 'sw-delta
;;; - :reset-fun. The function-name (symbol) for the reset-function (see above).
;;;   Default = 'sw-reset
;;;
;;; EXAMPLE
#|
(with-stopwatch (:print-delta t)
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
                             print-total
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
       ,@body
       (when ,print-total
         (format t "~&Total duration: ~a sec~%~%" (,delta-fun))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/svg-file->png
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-23
;;; 
;;; DESCRIPTION
;;; This function coverts a .svg file to a .png file via Inkscape. 
;;;
;;; ARGUMENTS
;;; The path to the svg-file. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :outfile. The output filename. Default = "/tmp/image.png"
;;; - :dpi. The dpi for the png. Default = 96
;;; - :width. The width of the resulting png file. When omitted, the width as
;;;   specified in the svg will be used, or -- when the height is given -- the
;;;   width will be derived proportionally from the height (see :height). 
;;; - :height. The height of the resulting png file. When omitted and no value
;;;   is set for :width, the size follow the specification in the svg-object.
;;;   If ommited and width is set, the height will be proportionally derived
;;;   from the width (inkscapes default behaviour). 
;;; 
;;; RETURN VALUE
;;; The path to the png file. 
;;;
;;; EXAMPLE


;;; SYNOPSIS
(defun svg-file->png (svg-file
                      &key
                        (outfile "/tmp/image.png")
                        (dpi 96)
                        width
                        height)
  ;;; ****
  (unless (integerp dpi)
    (error "utilities::svg-file->png: The dpi must be of type integer."))
  (unless (probe-file svg-file)
    (error "utilities::svg-file->png: The svg-file ~a does not exist."
           svg-file))
  (let ((command (list (get-apr-config :inkscape-command)
                       svg-file)))
    ;; add further elements to the command
    (when width
      (setf command (append command
                            (list "-w"
                                  (format nil "~a" width)))))
    (when height
      (setf command (append command
                            (list "-h"
                                  (format nil "~a" height)))))
    (setf command (append command
                          (list "--export-type"
                                "png"
                                "--export-png-color-mode"
                                "RGBA_8"
                                "--export-dpi"
                                (write-to-string dpi)
                                "-o"
                                outfile)))
    ;;; perform the conversion
    (apply #'shell command)
    outfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/svg-files->png
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-22
;;; 
;;; DESCRIPTION
;;; This function converts all svg files in a directory to png files. 
;;;
;;; ARGUMENTS
;;; none.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :indir. The input directory containing the svg files.
;;; - :outdir. The output directory for the pngs.
;;; - :in-extension. The filename extansions for the svg-files (without ".").
;;;   Default = "svg"
;;; - :dpi. The output dpi of the png files.
;;; Inherited from with-kernel:
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
;;; RETURN VALUE
;;; The output directory namestring. 
;;;
;;; SYNOPSIS
(defun svg-files->png (&key
                         indir outdir
                         (in-extension "svg")
                         (dpi 300)
                         width
                         height
                         ;; inherited from with-kernel
                         (num-workers (serapeum::count-cpus))
                         (kernel-name "apparence kernel")
                         (stopwatch? t)
                         (sw-start-accessor 'kernel-start)
                         (sw-delta-fun 'kernel-delta)
                         (sw-reset-fun 'kernel-reset))
  ;;; ****
  (unless (and indir outdir)
    (error "utilities::svg-files->png: Both indir and outdir have to be ~
            specified."))
  (unless (integerp dpi)
    (error "utilities::svg-files->png: The dpi must be of type integer."))
  (with-kernel (:num-workers num-workers
                :kernel-name kernel-name
                :stopwatch? stopwatch?
                :sw-start-accessor sw-start-accessor
                :sw-delta-fun sw-delta-fun
                :sw-reset-fun sw-reset-fun)
    (let* ((indir (trailing-slash indir))
           (outdir (trailing-slash outdir))
           (file-pattern (pathname (format nil "*.~a" in-extension)))
           (files (uiop:directory-files indir file-pattern))
           (file-counter 1)
           (num-files (length files)))
      (ensure-directories-exist outdir)
      (with-stopwatch ()
        (lparallel:pdotimes (i num-files)
          (with-stopwatch ()
            (let* ((infile (namestring (nth i files)))
                   (outfile (concatenate 'string outdir
                                         (pathname-name infile) ".png")))
              (svg-file->png infile :outfile outfile
                                    :dpi dpi
                                    :width width
                                    :height height)
              (format t "File: ~a (~a/~a) ~% ~
                     Duration: ~a sec~%"
                      outfile
                      file-counter num-files
                      (sw-delta))
              (incf file-counter))))
        (format t "Time elapsed: ~a seconds ~%" (sw-delta))))
    outdir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(secs->frames 1.0 :frame-rate 25) ;; => 25
|#
(defun secs->frames (secs &key
                            (frame-rate (get-apr-config :fps))
                            (round-fun #'round))
  ;;; ****
  (let ((res (* secs frame-rate)))
    (funcall round-fun res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(frames->secs 55) ;; => 11/5 (2.2)
|#
(defun frames->secs (frames &key
                              (frame-rate (get-apr-config :fps)))
  ;;; ****
  (/ frames frame-rate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (8bit->float 255) => 1.0

(defun 8bit->float (x)
  (if (and (numberp x) (<= 0 x))
      (rescale x 0 255 0.0 1.0)
      0))

;; (float->8bit .5) => 127

(defun float->8bit (x)
  (if (and (numberp x) (<= 0 x))
      (floor (rescale x 0.0 1.0 0 255))
      0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/interpolate-easing
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-29
;;; 
;;; DESCRIPTION
;;; This function interpolates between two values using an easing function (e.g.
;;; one of those proposed by Robert Penner). The x-range is defined by the
;;; :duration argument.
;;; The ease-fun must be a function accepting a number/float as its argument and
;;; returning a number/float between 0. and 1. to be used as a factor for the
;;; interpolation. You could use the easing functions from the easing package
;;; by Danielo Vidovic (e.g. ease:in-sine). 
;;;
;;; ARGUMENTS
;;; - The x-/time-axis value.
;;; - The start value.
;;; - The end value.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :duration. A number indicating the duration of the easing. This implies
;;;   the value range for the x-argument. Default = 1.0
;;; - :ease-fun. The function used for easing (see above).
;;;   Default = #'ease:in-sine
;;; 
;;; RETURN VALUE
;;; The interpolated value. 
;;;
;;; EXAMPLE
#|
(interpolate-easing 50 0 100 :duration 100
                             :ease-fun #'ease:in-bounce)
;; => 23.4375
|#
;;; SYNOPSIS
(defun interpolate-easing (x y-start y-end &key
                                             (duration 1.0)
                                             (ease-fun #'ease:in-sine))
  ;;; ****
  (+ y-start (* (- y-end y-start) (funcall ease-fun (/ x duration)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/image-seq->video
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-05
;;; 
;;; DESCRIPTION
;;; This function converts an image-seq (which is a collection of images, where
;;; each image represents a frame of a video) from a given path (which should
;;; be the directory containing the image files) to a video file using ffmpeg.
;;;
;;; NB: The files must be numerated according to their appearance in the video. 
;;;
;;; ARGUMENTS
;;; - The path to the directory containing the images.
;;; - The path to the video file which should be generated (usually a .mp4
;;;   file when using the default args).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :fps. The frame-rate for the conversion. Default = (get-apr-config :fps)
;;; - :glob-pattern. The search pattern for the files in the directory.
;;;   Default = "*.png"
;;; - :video-codec. The video codec, ffmpeg should use for conversion.
;;;   Default = "libx264"
;;; - :pixel-format. The pixel format ffmpeg should use for creating the video.
;;;   Default = "yuv420p"
;;; - :overwrite? When T, any existing file will be overwritten. Default = T
;;; - :verbose. Prints additional messages, when T.
;;;   Default = (get-apr-config :verbose)
;;; 
;;; RETURN VALUE
;;; The path to the outfile. 
;;;
;;; EXAMPLE
#|
(image-seq->video "/tmp/tl-seq/" "/tmp/test.mp4" :glob-pattern "*.jpg")
|#
;;; SYNOPSIS
(defun image-seq->video (path outfile &key
                          (fps (get-apr-config :fps))
                          (glob-pattern "*.png")
                          (video-codec "libx264")
                          (pixel-format "yuv420p")
                          (overwrite? t)
                          (verbose (get-apr-config :verbose)))
  ;;; ****
  (let* ((path (uiop:native-namestring
                (trailing-slash path)))
         (outfile (uiop:native-namestring outfile))
         (command (list (get-apr-config :ffmpeg-command)
                        "-framerate" (write-to-string fps)
                        "-pattern_type" "glob"
                        "-i" (concatenate 'string path glob-pattern)
                        "-c:v" video-codec
                        "-pix_fmt" pixel-format)))
    (when overwrite?
      (setf command (append command (list "-y"))))
    (setf command (append command (list outfile)))
    (when verbose
      (format t "~%Started converting image-seq in ~a to video ~a...~%"
              path outfile))
    (apply #'shell command)
    outfile))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/video->image-seq
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-05
;;; 
;;; DESCRIPTION
;;; This function converts a video file (e.g. a mp4) to a sequence of images,
;;; one for each frame, as specified by the :fps argument. 
;;;
;;; ARGUMENTS
;;; - The path to the video-file which should be converted.
;;; - The path to the directory where the resulting image files should be
;;;   located. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments
;;; - :outfile-pattern. The pattern which will be used to generate the output
;;;   file-names. Default = "%04d.png"
;;; - :fps. The frame-rate for the image conversion.
;;;   Default = (get-apr-config :fps)
;;; - :start. The start offset in the video (in seconds). Default = 0
;;; - :num-frames. The number of frames to generate (starting from the :start).
;;;   When NIL, all frames from :start will be converted to images. 
;;;   Default = NIL
;;; - :verbose. Prints additional messages, when T.
;;;   Default = (get-apr-config :verbose)
;;; 
;;; RETURN VALUE
;;; The path to the output directory.
;;;
;;; EXAMPLE
#|
(video->image-seq "/tmp/test.mp4" "/tmp/testseq/" :fps 25)
|#
;;; SYNOPSIS
(defun video->image-seq (vidfile outdir
                         &key
                           (outfile-pattern "%04d.png")
                           (fps (get-apr-config :fps))
                           ;; start offset in seconds
                           (start 0)
                           num-frames
                           (verbose (get-apr-config :verbose)))
  ;;; ****
  (unless (probe-file vidfile)
    (error "utilities::video->image-seq: The vidfile does not exist. "))
  (unless (numberp start)
    (error "utilities::video->image-seq: The start must be a number."))
  (unless (or (null num-frames) (integerp num-frames))
    (error "utilities::video->image-seq: The value of num-frames must either ~
            NIL or an integer."))
  (ensure-directories-exist outdir)
  (let* ((outdir (trailing-slash outdir))
         (outfiles (concatenate 'string outdir outfile-pattern))
         (command (list (get-apr-config :ffmpeg-command)
                        "-ss" (write-to-string start)
                        "-i" vidfile
                        "-vf" (concatenate 'string "fps="
                                           (write-to-string fps)))))
    (when num-frames
      (setf command (append command
                            (list "-frames:v" (write-to-string num-frames)))))
    (setf command (append command
                          (list outfiles)))
    (when verbose
      (format t "~%Started extracting images from video ~a to ~a...~%"
              vidfile outdir))
    (apply #'shell command)
    outdir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions/macros related to alists
;;; RP  Fri Apr  5 22:51:26 2024

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(alistp '((a . b))) ;; => T
(alistp '((a b))) ;; => NIL
|#
(defun alistp (thing)
  (and (listp thing)
       (every #'consp thing)
       (every #'(lambda (x)
                  (not (listp (cdr x))))
              thing)))

;;; additionally tests, if id already exists
(defmacro assoc-add (alist key value &key
                                       force?
                                       (test #'eql)
                                       (verbose (get-apr-config :verbose)))
  ;;; ****
  `(let ((current (assoc-value ,alist ,key :test ,test)))
     (cond ((and current ,force?)
            (when ,verbose
              (warn "utilities::assoc-add: Key ~a already exists. Overwrite."
                    ,key))
            (setf (assoc-value ,alist ,key :test ,test) ,value))
           ((null current)
            (setf (assoc-value ,alist ,key :test ,test) ,value))
           (t (error "utilities::assoc-add: Key ~a already exists." ,key)))
     ,alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assoc-remove (alist key &key (test #'eql))
  ;;; ****
  `(setf ,alist (remove-if #'(lambda (x)
                               (funcall ,test (car x) ,key))
                           ,alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(assoc-unique? '((a . b) (c . d) (a . x))) ;; => NIL
|#
(defun assoc-unique? (alist)
  ;;; ****
  (unless (alistp alist)
    (error "utilities::assoc-unique?: The alist is not of type alist"))
  (loop for item in alist
        with keys = '()
        do
           (when (find (car item) keys)
             (return nil))
           (push (car item) keys)
        finally
           (return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(list->alist '((1 bla)
               (3 blub)
               (2 bli))
             :sort t
             :unique t)
;; => ((1 . BLA) (2 . BLI) (3 . BLUB))
|#
(defun list->alist (lst &key unique sort)
  ;;; ****
  (unless (and (listp lst) (every #'(lambda (x)
                                      (and (listp x)
                                           (eq (length x) 2)))
                                  lst))
    (error "utilities::list->alist: The lst must be a list with at least one ~
            sublist containing exactly two elements."))
  (loop for item in lst
        with result = '()
        with keys = '()
        do
           (when (and unique (find (car item) keys))
             (error "utilities::list->alist: The key \"~a\" is already present ~
                     in the alist."
                    (car item)))
           (push (cons (first item) (second item)) result)
           (push (car item) keys)
        finally
           (if sort
               (return (sort result #'< :key #'car))
               (return result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/outfile-from-counter
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-24
;;; 
;;; DESCRIPTION

;;; This function formats a pathstring for an outfile based on an output
;;; directory and an incrementing counter (e.g. a frame index). 
;;;
;;; ARGUMENTS
;;; - The counter as an integer.
;;; - The outdirectory as a string. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :suffix. The outfile suffix (with the preceding dot), as a string.
;;;   Default = ".png"
;;; - :num-digits. The number of (zero) digits included in output filename. 
;;;   Default = 4.
;;; 
;;; RETURN VALUE
;;; The outfile path as a string. 
;;;
;;; EXAMPLE
#|
(outfile-from-counter 12 "/tmp/sequence/" :suffix ".jpg")
;; => "/tmp/sequence/0012.jpg"
|#
;;; SYNOPSIS

(defun outfile-from-counter (counter outdir &key
                                              (suffix ".png")
                                              (num-digits 4))
  ;;; ****
  (let ((outdir (trailing-slash outdir)))
    (format nil (concatenate 'string
                             "~a~"
                             (write-to-string num-digits)
                             ",'0d"
                             suffix)
            outdir counter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/perpendicular-distance
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-10-16
;;; 
;;; DESCRIPTION
;;; Calculate the perpendicular distance between a line and a point.
;;;
;;; ARGUMENTS
;;; - The line. Must be a two-item list, each item containing the xy-coordinates
;;;   of the starting and end point of the line. E.g.: '((0 0) (2 3))
;;; - The point. Must be a two-item list with the coordinates of the point.
;;;   E.g.: '(2 2)
;;; 
;;; 
;;; RETURN VALUE
;;; The perpendicular distance. 
;;;
;;; EXAMPLE
#|
(perpendicular-distance '((0 0) (10 0)) '(2 2))
;; => 2.0
|#
;;; SYNOPSIS
(defun perpendicular-distance (line point)
;;; ****
  (unless (and (listp line)
               (= 2 (length line))
               (every #'listp line)
               (every #'(lambda (x)
                          (= 2 (length x)))
                      line))
    (error "utilities::perpendicular-distance: line must be a two-item list ~
            with each item being a two-item list."))
  (unless (and (listp point)
               (= 2 (length point)))
    (error "utilities::perpendicular-distance: point must be a two-item list."))
  (let* ((p1 (first line))
         (p2 (second line))
         (x1 (first p1))
         (y1 (second p1))
         (x2 (first p2))
         (y2 (second p2))
         (x0 (first point))
         (y0 (second point)))
    (/ (abs
        (+ (- (* (- y2 y1) x0)
              (* (- x2 x1) y0))
           (- (* x2 y1) (* y2 x1))))
       (sqrt
        (+ (expt (- y2 y1) 2)
           (expt (- x2 x1) 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lisp
