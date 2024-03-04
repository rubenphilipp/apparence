;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; sequence1.lisp
;;;
;;; NAME
;;; sequence1
;;;
;;; DESCRIPTION
;;; Testing lparallel with NEW apparence classes and cloned cm.
;;; Adding some stuff.
;;; 
;;; NOTE: cloning is more expensive than creating a new projection surface (cf.
;;;       lparallel-test9.lisp)
;;;       additional computing time (on my MBP): approx. +20 secs (when
;;;       rendering 200 frames)
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-01
;;;
;;; $$ Last modified:  15:07:15 Mon Mar  4 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(unless (find-package :serapeum)
  (ql:quickload :serapeum))

(init-kernel (serapeum:count-cpus))

(let* ((mantle-height-px 2770)
       (mantle-width-px 8192)
       (img-orig (make-rgb-image 2000 1381
                                 :initial-color (imago::make-color 23 90 134)))
       (pn (make-projection img-orig
                            :projection-height 40.36670443321064d0))
       (cm (make-cylinder-mantle 40.36670443321064d0
                                 :surface-diameter 38
                                 :width mantle-width-px
                                 :height mantle-height-px))
       (outdir "/tmp/seq-test/")
       (azim-env '(0 0 20 180 50 100 75 -20 100 24))
       (y-env '(0 -100 30 0 60 40 70 40 85 0 100 10))
       (frames 200)
       (y-env-sc (rescale-envelope y-env :x-min 0 :x-max frames))
       (azim-env-sc (rescale-envelope azim-env :x-min 0
                                               :x-max frames))
       (render-start-time (get-universal-time)))
  (ensure-directories-exist outdir)
  (lparallel:pdotimes (i frames)
    (let* ((azim (interpl i azim-env-sc :base 1.12))
           (y (floor (interpl i y-env-sc :base 1.16)))
           (start-time (get-universal-time))
           (outfile (format nil "~a~4,'0d.jpg" outdir i))
           (new-width (max .5 (* (projection-width pn)
                                 (abs (sin i)))))
           ;; this cloning is okay as it does not excessively strain performance
           (ps (clone cm)))
      (put-it-circular ps pn azim y :canvas-origin 0
                                    :image-origin 0.5
                                    :src-x (floor (- (/ (projection-width pn)
                                                        2)
                                                       (/ new-width 2)))
                                    :width new-width)
      (write-jpg ps :outfile outfile)
      ;;(write-png ps :outfile outfile)
      (format t "File: ~a~%~
                 Duration: ~a sec~%"
              outfile
              (- (get-universal-time) start-time))))
  ;;(write-jpg cm :outfile (concatenate 'string outdir "cm.jpg"))
  (format t "Time elapsed: ~a sec ~%"
          (- (get-universal-time) render-start-time)))

(shutdown-kernel)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sequence1.lisp
