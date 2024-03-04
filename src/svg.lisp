;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apr/svg
;;; NAME
;;; svg
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; PURPOSE
;;; Implementation of svg facilities.
;;;
;;; NB: This module relies in part on the cl-svg library. 
;;;
;;; CLASS HIERARCHY
;;; no classes defined.
;;; some methods relate to cl-svg::svg-toplevel and others. 
;;;
;;; $$ Last modified:  17:11:40 Mon Mar  4 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* svg/write-svg
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; DESCRIPTION
;;; This method writes a cl-svg::svg-toplevel object to a file. 
;;;
;;; ARGUMENTS
;;; The cl-svg::svg-toplevel object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :outfile. The output filename. Default = "/tmp/image.svg"
;;; 
;;; RETURN VALUE
;;; The output filename. 
;;;
;;; EXAMPLE
#|
(let ((canvas (cl-svg::make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                         :width 400
                                         :height 300)))
  (cl-svg:draw canvas
      (:rect :x 10 :y 10
             :width 100 :height (* 100 4/3)
             :fill "rgba(90,90,90,1)"))
  (write-svg canvas))
;; => "/tmp/image.svg"
|#
;;; SYNOPSIS
(defmethod write-svg ((svg cl-svg::svg-toplevel)
                      &key
                        (outfile "/tmp/image.svg"))
  ;;; ****
  (with-open-file (s outfile
                     :direction :output
                     :if-exists :supersede)
    (cl-svg:stream-out s svg))
  outfile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* svg/svg->png
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; DESCRIPTION
;;; This method converts a cl-svg::svg-toplevel object to a png file.
;;;
;;; NB: This command makes use of inkscape's cli-interface. 
;;;
;;; ARGUMENTS
;;; The cl-svg::svg-toplevel object. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :outfile. The output filename. Default = "/tmp/image.png"
;;; - :tmp-dir. A path to a directory where the temp-files are stored.
;;;   Default = (get-apr-config :default-tmp-dir)
;;; - :width. The width of the resulting png file. When omitted, the width as
;;;   specified in the svg will be used, or -- when the height is given -- the
;;;   width will be derived proportionally from the height (see :height). 
;;; - :height. The height of the resulting png file. When omitted and no value
;;;   is set for :width, the size follow the specification in the svg-object.
;;;   If ommited and width is set, the height will be proportionally derived
;;;   from the width (inkscapes default behaviour). 
;;; 
;;; RETURN VALUE
;;; The output filename. 
;;;
;;; EXAMPLE
#|
(let ((canvas (cl-svg::make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                         :width 400
                                         :height 300)))
  (cl-svg:draw canvas
      (:rect :x 10 :y 10
             :width 100 :height (* 100 4/3)
             :fill "rgba(90,90,90,1)"))
  (svg->png canvas))
;; => "/tmp/image.png"
|#
;;; SYNOPSIS
(defmethod svg->png ((svg cl-svg::svg-toplevel)
                     &key
                       (outfile "/tmp/image.png")
                       (tmp-dir (get-apr-config :default-tmp-dir))
                       width
                       height)
  ;;; ****
  (let* ((tmpfile (format nil "~a~a.svg"
                          (trailing-slash tmp-dir)
                          (gensym "svg-png")))
         (command (list (get-apr-config :inkscape-command)
                        tmpfile)))
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
                          (list "-o" outfile)))
    ;;; perform the conversion
    (ensure-directories-exist tmp-dir)
    (write-svg svg :outfile tmpfile)
    (apply #'shell command)
    (delete-file tmpfile)
    outfile))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF svg.lisp
