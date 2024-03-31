;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; sequence2.lisp
;;;
;;; NAME
;;; sequence2
;;;
;;; DESCRIPTION
;;; An example using the with-timeline macro and easing functions.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-30
;;;
;;; $$ Last modified:  21:10:37 Sun Mar 31 2024 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(let* ((ps-w-px 500)
       (ps-h-px 300)
       (img-orig (make-rgb-image 50 50
                                 :initial-color (make-color 40 90 18 255)))
       ;;; the image as a projection object
       (pn (make-projection img-orig
                            :projection-height 5))
       ;;; a blueprint of the projection surface
       (ps (make-projection-surface :surface-width 50 :surface-height 30
                                    :width ps-w-px :height ps-h-px))
       ;;; the duration of the whole sequence in seconds
       (dur-sec 5.0)
       ;;; the duration of the sequence in frames (based on apr's global frame
       ;;; rate)
       (dur-frames (secs->frames dur-sec))
       (frame-counter 1)
       (outdir "/tmp/tl-seq/"))
  (ensure-directories-exist outdir)
  (with-kernel ()
    (lparallel:pdotimes (i dur-frames)
      (with-stopwatch ()
        (let ((outfile (format nil "~a~4,'0d.jpg" outdir i))
              (ps2 (clone ps))
              (pn-x '(2 15))
              (pn-y '(5 20)))
          (flet ((change-alpha (pn alpha)
                   (let ((tmp-pn (clone pn)))
                     (imago::do-image-pixels ((data tmp-pn) color x y)
                       (setf color (make-color (imago::color-red color)
                                               (imago::color-green color)
                                               (imago::color-blue color)
                                               alpha)))
                     tmp-pn)))
            (with-timeline ((frames->secs i) .2 :end 5.0)
              (let ((tmp-pn
                      (change-alpha pn
                                    ;; fade in and out
                                    (if (< tl-time (/ tl-duration 2))
                                        (round
                                         (interpolate-easing
                                          tl-time
                                          0 255
                                          :duration tl-duration
                                          :ease-fun #'ease:out-exp))
                                        (round
                                         (interpolate-easing
                                          tl-time
                                          255 0
                                          :duration tl-duration
                                          :ease-fun #'ease:in-exp))))))
                (put-it ps2 tmp-pn :dest-x (interpolate-easing tl-time
                                                               (first pn-x)
                                                               (second pn-x)
                                                               :duration
                                                               tl-duration
                                                               :ease-fun
                                                               #'ease:in-bounce)
                                   :dest-y
                                   (interpolate-easing tl-time
                                                       (first pn-y)
                                                       (second pn-y)
                                                       :duration
                                                       tl-duration
                                                       :ease-fun
                                                       #'ease:in-quad)))))
          (write-jpg ps2 :outfile outfile)
          (format t "File: ~a~%~
                             Frame: ~a/~a~%~
                             Duration: ~a sec~%"
                  outfile
                  frame-counter dur-frames
                  (sw-delta))
          (incf frame-counter))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sequence2.lisp
