;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; convert svgs in directory to png files

(ql:quickload :apparence)

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (find-package :serapeum)
  (ql:quickload :serapeum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(init-kernel (serapeum:count-cpus))

(let* ((indir (trailing-slash
               (progn
                 (format t "~%Input directory: ")
                 (read-line))))
       (outdir (trailing-slash
                (progn
                  (format t "~%Output directory: ")
                  (read-line))))
       (in-extension "svg")
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
          (apply #'shell (list (apr::get-apr-config :inkscape-command)
                               infile
                               "--export-png-color-mode"
                               "RGBA_8"
                               "-o"
                               outfile))
          (format t "File: ~a (~a/~a) ~% ~
                     Duration: ~a sec~%"
                  outfile
                  file-counter num-files
                  (sw-delta))
          (incf file-counter))))
    (format t "Time elapsed: ~a seconds ~%" (sw-delta))))

(shutdown-kernel)
