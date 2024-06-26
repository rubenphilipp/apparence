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
;;; $$ Last modified:  23:00:41 Tue May 21 2024 CEST
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

;;; this is a simplified shorthand-version of svg->png
;;; RP  Thu Apr 25 22:33:21 2024
(defmethod write-png ((img cl-svg::svg-toplevel)
                      &key (outfile "/tmp/image.png"))
  ;;; ****
  (svg->png img :outfile outfile))

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
;;; - :args. Further keyword arguments to be passed to cl-svg:make-svg-toplevel
;;;   as a list. Example: '(:style "background-color:black;")
;;; 
;;; RETURN VALUE
;;; The new cl-svg:svg-toplevel object. 
;;;
;;; EXAMPLE
#|
(make-svg-toplevel :width 100
                   :height 200
                   :args '(:style "background-color: #ffffff;"))
;; => #<CL-SVG:SVG-1.1-TOPLEVEL {701AC30883}>
|#
;;; SYNOPSIS
(defun make-svg-toplevel (&key
                            (class 'cl-svg:svg-1.1-toplevel)
                            width height args)
  ;;; ****
  (unless (or (null args) (listp args))
    (error "svg::make-svg-toplevel: The args must be either nil or a list."))
  (apply #'cl-svg:make-svg-toplevel
         (append (list class :width width :height height)
                 args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* svg/xy-list->polygon-points
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-25
;;; 
;;; DESCRIPTION
;;; This function formats an xy-list to a string of svg polygon points. 
;;;
;;; ARGUMENTS
;;; An xy-list.
;;; 
;;; RETURN VALUE
;;; The polygon-points as a list. 
;;;
;;; EXAMPLE
#|
(xy-list->polygon-points '((0 5) (10 5) (15 8)))
;;; => "0,5 10,5 15,8"
|#
;;; SYNOPSIS
(defun xy-list->polygon-points (xy-list)
  ;;; ****
  (unless (xy-list-p xy-list)
    (error "svg::xy-list->polygon-points: The xy-list is malformed."))
  (format nil "~{~{~a,~a~}~^ ~}" xy-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF svg.lisp
