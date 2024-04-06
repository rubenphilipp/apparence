;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; sequence3.lisp
;;;
;;; NAME
;;; sequence3
;;;
;;; DESCRIPTION
;;; An example of a sequence using a video file as material. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-06
;;;
;;; $$ Last modified:  15:39:03 Sat Apr  6 2024 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(set-apr-config :fps 30)

(let* ((ifs (make-image-file-seq-from-video (path-from-same-dir
                                             "test1.mp4")))
       (ifs-dur (duration ifs))
       (dur-sec 5.0)
       (dur-frames (secs->frames dur-sec))
       (outdir (concatenate 'string
                            (get-apr-config :default-tmp-dir)
                            "sequence3/"))
       (frame-counter 1)
       (ps-w-px 800)
       (ps-h-px 500)
       (ps (make-projection-surface :surface-width 80
                                    :surface-height 50
                                    :width ps-w-px
                                    :height ps-h-px)))
  (ensure-directories-exist outdir)
  (with-kernel ()
    (lparallel:pdotimes (i dur-frames)
      (with-stopwatch ()
        (let* ((outfile (format nil "~a~4,'0d.jpg" outdir i))
               (tmp-ps (clone ps))
               (vid-img (get-image ifs (mod (frames->secs i) ifs-dur)
                                   :in-seconds t))
               (pn nil))
          (when vid-img
            (setf pn (make-projection vid-img :projection-height 50))
            (put-it tmp-ps pn :dest-x 5 :dest-y 5))
          (write-jpg tmp-ps :outfile outfile)
          (format t "File: ~a~%~
                             Frame: ~a/~a~%~
                             Duration: ~a sec~%"
                  outfile
                  frame-counter dur-frames
                  (sw-delta))
          (incf frame-counter))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sequence3.lisp
