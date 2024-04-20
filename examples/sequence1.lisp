;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; sequence1.lisp
;;;
;;; NAME
;;; sequence1
;;;
;;; DESCRIPTION
;;; This example shows how to project a sequence of generated images onto the
;;; mantle of a cylindrical surface.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-01
;;;
;;; $$ Last modified:  21:18:26 Sat Apr 20 2024 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;(set-apr-config :verbose t)

(let* (;; set the data for the sequence before starting the parallel-process
       (mantle-height-px 2770)
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
       (y-env '(0 -5 30 0 60 20 70 6 85 0 100 -10))
       (frames 100)
       ;; this helps discerning the global progress
       (frame-counter 1)
       (y-env-sc (rescale-envelope y-env :x-min 0 :x-max frames))
       (azim-env-sc (rescale-envelope azim-env :x-min 0
                                               :x-max frames)))
  (ensure-directories-exist outdir)
  ;; this starts the lparallel-kernel for parallely processing the images
  (with-kernel ()
    ;; this starts a "stopwatch" for the whole process
    (with-stopwatch ()
      ;; this is the main image generating loop
      (lparallel:pdotimes (i frames)
        ;; this starts a "stopwatch" for each image generating subprocess
        (with-stopwatch ()
          (let* ((azim (interpl i azim-env-sc :base 1.12))
                 (y (interpl i y-env-sc :base 1.16))
                 (start-time (get-universal-time))
                 (outfile (format nil "~a~4,'0d.jpg" outdir i))
                 (new-width (* (projection-width pn) .3))
                 ;; this cloning is okay as it does not excessively strain
                 ;; performance
                 (ps (clone cm)))
            ;; just process this frame if the outfile does not yet exist
            (if (probe-file outfile)
                (format t "File: ~a~%~
                           Frame: ~a/~a~%~
                           File already exists. Skipping.~%"
                        outfile
                        frame-counter frames)
                (progn
                  (compose-circular ps pn azim y :canvas-origin 0
                                                :image-origin 0.5
                                                :src-x
                                                (- (/ (projection-width
                                                       pn)
                                                      2)
                                                   (/ new-width 2))
                                                :width new-width)
                  ;; note: jpgs are faster than pngs
                  (write-jpg ps :outfile outfile)
                  ;;(write-png ps :outfile outfile)
                  (format t "File: ~a~%~
                             Frame: ~a/~a~%~
                             Duration: ~a sec~%"
                          outfile
                          frame-counter frames
                          (sw-delta))))
            (incf frame-counter))))
      (format t "Time elapsed: ~a sec ~%"
              (sw-delta)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sequence1.lisp
