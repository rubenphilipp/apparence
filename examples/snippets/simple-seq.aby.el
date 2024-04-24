;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; simple-seq.aby.el
;;;
;;; NAME
;;; simple-seq
;;;
;;; DESCRIPTION
;;; This snippet inserts some code for working with image sequences. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-24
;;;
;;; $$ Last modified:  21:47:30 Wed Apr 24 2024 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((fragment "simple-seq.lisp")
       (static-rep '())
       (ask-rep '((fps-aux nil "FPS (25): ")
                  (ps-width "{{ps-width}}" "projection-surface width: ")
                  (ps-height "{{ps-height}}" "projection-surface height: ")
                  (dur-secs-aux nil "Duration in secs (1.0): ")
                  (outdir-aux nil "Output directory (/tmp/seq/): ")))
       (dynamic-rep '((fps "{{fps}}"
                           '(lambda (reps)
                              (let ((the-fps
                                     (alist-get 'fps-aux reps)))
                                (if (string= "" the-fps)
                                    "25"
                                  the-fps))))
                      (dur-secs "{{dur-secs}}"
                                '(lambda (reps)
                                   (let ((the-dur
                                          (alist-get 'dur-secs-aux reps)))
                                     (if (string= "" the-dur)
                                         "1.0"
                                       the-dur))))
                      (outdir "{{outdir}}"
                              '(lambda (reps)
                                 (let ((the-outdir
                                        (alist-get 'outdir-aux reps)))
                                   (if (string= "" the-outdir)
                                       "/tmp/seq/"
                                     the-outdir)))))))
  (aby-instruct fragment
                static-rep
                ask-rep
                dynamic-rep))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF simple-seq.aby.el
