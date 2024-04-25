(in-package :apparence)

(set-apr-config :fps {{fps}})

(let* ((width {{ps-width}})
       (height {{ps-height}})
       (dur-secs {{dur-secs}})
       (dur-frames (secs->frames dur-secs))
       ;; this could serve as a blueprint
       (ps (make-projection-surface :width width
                                    :height height
                                    :x-scaler 1.0))
       (outdir "{{outdir}}"))
  (ensure-directories-exist outdir)
  (with-stopwatch (:print-total t)
    (with-kernel ()
      (do-frames (f dur-frames :start 0 :verbose t)
        (let* ((outfile (outfile-from-counter f outdir :suffix ".png"))
               ;; this could be used for the current frame
               (tmp-ps (clone ps)))
          ;; if you want to avoid re-rendering frames
          (unless (probe-file outfile)
            ;; main code for the current frame
            ;; ----
            (write-png tmp-ps :outfile outfile)
            (format t "File: ~a~%" outfile)))))
    ;; convert the image sequence to a video file
    (format t "~&Converting to video... ~%")
    (image-seq->video outdir (format nil "~avid.mp4"
                                     (trailing-slash outdir))
                      :glob-pattern "*.png")))
       
