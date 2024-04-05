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
;;; $$ Last modified:  17:55:40 Fri Apr  5 2024 CEST
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
;;;
;;; further arguments are inherited from svg-file->png.
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
                       (dpi 96)
                       (tmp-dir (get-apr-config :default-tmp-dir))
                       width
                       height)
  ;;; ****
  (let* ((tmpfile (format nil "~a~a.svg"
                          (trailing-slash tmp-dir)
                          (get-random-uuid))))
    ;;; perform the conversion
    (ensure-directories-exist tmp-dir)
    (write-svg svg :outfile tmpfile)
    (svg-file->png tmpfile :outfile outfile
                           :dpi dpi
                           :width width
                           :height height)
    (delete-file tmpfile)
    outfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* svg/make-svg-toplevel
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-04
;;; 
;;; DESCRIPTION
;;; A shorthand for the cm-svg:make-svg-toplevel method. 
;;;
;;; ARGUMENTS
;;; none.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :class. The cl-svg class. Default = 'cl-svg:svg-1.1-toplevel
;;; - :width. The width of the svg.
;;; - :height. The height of the svg.
;;; - :viewbox. The svg viewbox coordinates (e.g. "0 0 700 700")
;;; 
;;; RETURN VALUE
;;; The new cl-svg:svg-toplevel object. 
;;;
;;; EXAMPLE
#|
(make-svg-toplevel :width 100
                   :height 200)
;; => #<CL-SVG:SVG-1.1-TOPLEVEL {7006DD7CA3}>
|#
;;; SYNOPSIS
(defun make-svg-toplevel (&key
                            (class 'cl-svg:svg-1.1-toplevel)
                            width height viewbox)
  ;;; ****
  (cl-svg:make-svg-toplevel class
                            :width width
                            :height height
                            :viewbox viewbox))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF svg.lisp
