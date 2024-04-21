;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apparence tests
;;; NAME
;;; apparence tests
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; PURPOSE
;;; Regression test suite for apparence. 
;;;
;;;
;;; $$ Last modified:  21:44:18 Sun Apr 21 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :apparence.tests
  (:use :cl :apparence :fiveam)
  (:shadow :test)
  (:export :run-tests))

(in-package :apparence.tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite apparence)
(in-suite apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro test (name &body body)
  `(5am:test ,name
             ,@body))

(defmacro test-pathname (path)
  `(namestring (asdf::SYSTEM-RELATIVE-PATHNAME :apparence
                                               (concatenate 'string
                                                            "tests/"
                                                            ,path))))

(defun run-tests ()
  (run! 'apparence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example
#|
;;; test trailing slash
;;; RP  Sat Jul 15 14:43:24 2023
(test test-trailing-slash
(is (equal "/trailing/test/"
(colporter::trailing-slash "/trailing/test"))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; test-canvas-simple
;;; RP  Wed Feb 28 22:00:38 2024
(test test-canvas-simple
      (let ((cv (make-instance 'canvas :width 100 :height 200)))
        (is (= 100 (width cv)))))

;;; test-write-png-simple
;;; RP  Wed Feb 28 23:08:43 2024
(test test-write-png-simple
      (let ((cv (make-instance 'canvas :width 300 :height 200
                                       :color '(255 255 255)))
            (img (apr::make-rgb-image 50 100
                                      :initial-color
                                      (imago::make-color 100 233 90))))
        (apr:copy (data cv) img)
        (apr:write-png cv :outfile "/tmp/test-canvas.png"))
      (is (probe-file "/tmp/test-canvas.png")))

;;; test-compose-simple
;;; RP  Thu Feb 29 14:02:15 2024
(test test-compose-simple
      (let* ((cv (apr:make-canvas 300 200 :color '(0 0 0 0)))
             (img (apr::make-rgb-image 50 100
                                       :initial-color
                                       (imago::make-color 100 233 90))))
        (apr:compose cv img :dest-x 20)
        (apr:write-png cv :outfile "/tmp/cv-test.png"))
      (is (probe-file "/tmp/cv-test.png")))

;;; test-compose-elaborate1
;;; RP  Tue Mar 26 00:06:15 2024
(test test-compose-elaborate1
      (let* ((cv (apr:make-canvas 300 200 :color '(0 0 0 0)))
             (img (apr::make-rgb-image 50 100
                                       :initial-color
                                       (imago::make-color 100 233 90))))
        (apr:compose cv img :dest-x 20 :width 20 :height 30 :src-x 5 :src-y 10)
        (apr:write-png cv :outfile "/tmp/cv-test-ela1.png"))
      (is (probe-file "/tmp/cv-test-ela1.png")))

;;; test cylinder-mantle
;;; RP  Mon Feb 26 17:53:24 2024
(test test-cylinder-mantle
      (let ((cm (apr::make-cylinder-mantle 40 :surface-diameter 10
                                              :width 2000
                                              :height 4000
                                              :id 'visiodrom)))
        (is (numberp (surface-diameter cm))
            (numberp (surface-width cm))
            (= (surface-height cm) 40)
            (= (y-scaler cm) 1/100)
            (= (width cm) 2000))))

;;; test-get-coordinates
;;; RP  Thu Feb 29 14:42:06 2024
(test test-get-coordinates
      (let* ((cm (apr:make-cylinder-mantle 10 :surface-diameter 10
                                              :width 100
                                              :height 200))
             (coords (apr:get-coordinates cm 90 :azimuth-origin-offset 0)))
        (is (equal coords '(5.0d0 0.0d0)))))


;;; test-compose-circular
;;; RP  Thu Feb 29 15:42:13 2024
(test test-compose-circular
      (let* ((cv (apr:make-canvas 1000 500 :color '(0 0 0 0)))
             (img (apr:make-rgb-image 250 200
                                      :initial-color
                                      (apr::make-color 233 200 188)))
             (outfile "/tmp/compose-circular.jpg"))
        (apr:compose-circular cv img 350 0)
        (apr::write-jpg cv :outfile outfile)
        (is (and (typep (data cv) 'apr:image)
                 (probe-file outfile)))))

;;; test-image1
;;; RP  Thu Feb 29 21:19:14 2024
(test test-image1
      (let ((image (make-instance 'apr:image
                                  :data (imago::make-rgb-image 20 20))))
        (setf (data image) (imago::make-rgb-image 20 50))
        (setf (width image) 300)
        (setf (height image) 100)
        (is (and (= 300 (width image))
                 (= 100 (height image))
                 (= 300 (imago::image-width (data image)))
                 (= 100 (imago::image-height (data image)))))))

;;; test-copy1
;;; RP  Thu Feb 29 21:40:17 2024
(test test-copy1
      (let* ((img1 (make-instance 'apr:image
                                  :data (imago::make-rgb-image
                                         200 200
                                         (imago::make-color 232 130 232))))
             (img2 (apr:make-image (imago::make-rgb-image
                                    20 20
                                    (imago::make-color 0 0 0)))))
        (apr:copy img1 img2)
        (is (= 200 (imago::image-width (data img1))))))

;;; test-write-png-image
;;; RP  Thu Feb 29 21:40:26 2024
(test test-write-png-image
      (let* ((img1 (apr:make-rgb-image 200 200
                                       :initial-color
                                       (apr::make-color 232 130 232)))
             (img2 (apr:make-image (imago::make-rgb-image
                                    20 20
                                    (imago::make-color 0 0 0)))))
        (apr:copy img1 img2 :dest-x 30)
        (apr:write-png img1 :outfile "/tmp/test.png")
        (is (probe-file "/tmp/test.png"))))

;;; test-images-from-specific-files
;;; RP  Thu Feb 29 22:25:15 2024
(test test-images-from-specific-files
      (let ((img-png (apr:make-image-from-png
                      (test-pathname "image.png")))
            (img-jpg (apr:make-image-from-jpg
                      (test-pathname "image.jpg"))))
        (is (and (typep img-png 'apr:image)
                 (typep img-jpg 'apr:image)))))

;;; test-image-from-file
;;; RP  Thu Feb 29 22:37:19 2024
(test test-image-from-file
      (let ((img1 (apr:make-image-from-file
                   (test-pathname "image.png")))
            (img2 (apr:make-image-from-file
                   (test-pathname "image.jpg"))))
        (is (and (typep img1 'apr:image)
                 (typep img2 'apr:image)))))

;;; test-make-ps-simple1
;;; RP  Fri Mar  1 21:55:19 2024
(test test-make-ps-simple1
      (let ((ps (apr:make-projection-surface :surface-width 20
                                             :surface-height 40
                                             :x-scaler 10
                                             :y-scaler 10)))
        (setf (width ps) 4)
        (setf (height ps) 8)
        (setf (surface-width ps) 20)
        (setf (surface-height ps) 40)
        (setf (x-scaler ps) 20)
        (is (and (typep ps 'projection-surface)
                 (= (width ps) 2)
                 (= (height ps) 4)
                 (= (surface-width ps) 40)
                 (= (surface-height ps) 40)
                 (= (x-scaler ps) 20)
                 (= (y-scaler ps) 10)))))

;;; test-ps-setters1
;;; RP  Fri Mar  1 23:12:14 2024
(test test-ps-setters1
      (let ((ps (apr:make-projection-surface :surface-width 20
                                             :surface-height 30
                                             :x-scaler 10.5
                                             :y-scaler 10.5)))
        (setf (width ps) 40)
        (setf (surface-width ps) 20)
        (setf (y-scaler ps) 4)
        (setf (x-scaler ps) 10)
        (setf (height ps) 50)
        (setf (width ps) 250)
        (is (and (= 2500 (surface-width ps))
                 (= 200 (surface-height ps))
                 (= 10 (x-scaler ps))
                 (= 4 (y-scaler ps))
                 (= 250 (width (data ps)))
                 (= 50 (height (data ps)))))))

;;; test-projection-simple1
;;; RP  Sat Mar  2 18:07:57 2024
(test test-projection-simple1
      (let ((pn (make-instance 'projection :projection-width 400
                                           :projection-height 600
                                           :x-scaler 2
                                           :y-scaler 2
                                           :data
                                           (imago::make-rgb-image 200 300))))
        (setf (width pn) 400)
        (setf (x-scaler pn) 3)
        (setf (y-scaler pn) 4)
        (setf (height pn) 200)
        (setf (projection-width pn) 1000)
        (setf (projection-height pn) 500)
        (setf (width pn) 300)
        (is (and (= (width pn) 300)
                 (= (height pn) 125)
                 (= (x-scaler pn) 3)
                 (= (y-scaler pn) 4)
                 (= (projection-width pn) 900)
                 (= (projection-height pn) 500)))))

;;; test-make-projection
;;; RP  Sat Mar  2 18:21:00 2024
(test test-make-projection
      (let* ((img1 (make-image-from-file
                    (test-pathname "image-200x200.png")))
             (pn1 (make-projection img1 :x-scaler 2
                                        :y-scaler 3)))
        (setf (projection-width pn1) 800)
        (setf (height pn1) 100)
        (resize pn1 200 200)
        (is (and (= (projection-width pn1) 400)
                 (= (projection-height pn1) 600)
                 (= (x-scaler pn1) 2)
                 (= (y-scaler pn1) 3)
                 (= (width pn1) 200)
                 (= (height pn1) 200)))))

;;; test-copy-projection
;;; RP  Sat Mar  2 19:50:38 2024
(test test-copy-projection
      (let* ((img1 (make-rgb-image 200 200
                                   :initial-color
                                   (imago::make-color 210 180 90)))
             (img2 (make-rgb-image 100 100
                                   :initial-color
                                   (imago::make-color 120 120 90)))
             (pn1 (make-projection img1
                                   :x-scaler 4
                                   :y-scaler 4))
             (pn2 (make-projection img2
                                   :x-scaler 2
                                   :y-scaler 2)))
        (copy pn1 pn2 :dest-x 300 :dest-y 300
                      :width 25 :height 25
                      :src-x 50 :src-y 50)
        (is (typep pn1 'projection))))

;;; test-make-projection2
;;; RP  Sat Mar  2 21:07:43 2024
(test test-make-projection2
      (let* ((img (make-rgb-image 200 400
                                  :initial-color
                                  (imago::make-color 210 180 90)))
             (pn1 (make-projection img
                                   :x-scaler 2))
             (pn2 (make-projection img
                                   :projection-height 800)))
        (is (and (= (projection-height pn1) 800)
                 (= (projection-width pn2) 400)))))

;;; test-pn-scale
;;; RP  Sat Mar  2 21:40:53 2024
(test test-pn-scale
      (let* ((img (make-rgb-image 200 400
                                  :initial-color
                                  (imago::make-color 210 180 90)))
             (pn (make-projection img
                                  :x-scaler 2)))
        (scale pn 2 2)
        (is (and (= (projection-width pn) 800)
                 (= (x-scaler pn) 4)
                 (= (projection-height pn) 1600)
                 (= (y-scaler pn) 4)))))


;;; test-compose-ps-pn
;;; RP  Sat Mar  2 23:37:50 2024
(test test-compose-ps-pn
      (let* ((cm (make-projection-surface :surface-width (* 38 pi)
                                          :surface-height 40.36670443321064d0
                                          :x-scaler (/ (* 38.0 pi) 8192)
                                          :y-scaler
                                          (/ 40.36670443321064d0 2770)))
             (img-orig (make-rgb-image 2000 1381 :initial-color
                                       (imago::make-color 222 100 189)))
             (pn (make-projection img-orig
                                  :projection-height 40.36670443321064d0)))
        (compose cm pn :src-y 0 :dest-y 37.5 :dest-x 100.15)
        (when (probe-file "/tmp/compose-ps.jpg")
          (delete-file "/tmp/compose-ps.jpg"))
        (write-jpg cm :outfile "/tmp/compose-ps.jpg")
        (is (probe-file "/tmp/compose-ps.jpg"))))

;;; test-compose-circular-pn
;;; RP  Sun Mar  3 17:44:23 2024
(test test-compose-circular-pn
      (let* ((cm (make-projection-surface :surface-width (* 38 pi)
                                          :surface-height 40.36670443321064d0
                                          :x-scaler (/ (* 38.0 pi) 8192)
                                          :y-scaler
                                          (/ 40.36670443321064d0 2770)))
             (img-orig (make-rgb-image 2000 1381
                                       :initial-color (imago::make-color
                                                       231 23 23 255)))
             (pn (make-projection img-orig
                                  :projection-height 40.36670443321064d0))
             (outfile "/tmp/test-compose-circular.jpg"))
        (compose-circular cm pn 400 -20
                          :src-x 5 :src-y 5
                          :width 30 :height 30)
        (when (probe-file outfile)
          (delete-file outfile))
        (write-jpg cm :outfile outfile)
        (is (probe-file outfile))))

;;; test-svg->png
;;; RP  Mon Mar  4 16:47:03 2024
(test test-svg->png
      (let ((canvas (cl-svg::make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                               :width 400
                                               :height 300)))
        (cl-svg:draw canvas
            (:rect :x 10 :y 10
                   :width 100 :height (* 100 4/3)
                   :fill "rgba(90,90,90,1)"))
        (svg->png canvas :outfile "/tmp/svg.png")
        (is (probe-file "/tmp/svg.png"))))

;;; test-image-from-svg
;;; RP  Mon Mar  4 16:58:42 2024
(test test-image-from-svg
      (let ((canvas (cl-svg::make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                               :width 400
                                               :height 300))
            (outfile "/tmp/image.png")
            (image nil))
        (cl-svg:draw canvas
            (:rect :x 10 :y 10
                   :width 100 :height (* 100 4/3)
                   :fill "rgba(90,90,90,1)"))
        (setf image (make-image-from-svg canvas))
        (when (probe-file outfile)
          (delete-file outfile))
        (write-png image :outfile outfile)
        (is (probe-file outfile))))

;;; test-write-svg
;;; RP  Mon Mar  4 17:11:50 2024
(test test-write-svg
      (let ((canvas (cl-svg::make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                               :width 400
                                               :height 300))
            (outfile "/tmp/test-write.svg"))
        (cl-svg:draw canvas
            (:rect :x 10 :y 10
                   :width 100 :height (* 100 4/3)
                   :fill "rgba(90,90,90,1)"))
        (when (probe-file outfile)
          (delete-file outfile))
        (write-svg canvas :outfile outfile)
        (is (probe-file outfile))))

;;; test-make-svg-toplevel
;;; RP  Mon Mar  4 17:17:24 2024
(test test-make-svg-toplevel
      (let ((result (make-svg-toplevel :width 100 :height 200)))
        (is (typep result 'cl-svg::svg-toplevel))))

;;; test-pcg-random
(test test-pcg-random
      (let ((random1 nil)
            (random2 nil)
            (random3 nil))
        (reset-pcg)
        (setf random1 (pcg-random 10))
        (reset-pcg)
        (setf random2 (pcg-random 10))
        (setf random3 (pcg-random 10 :reset t))
        (is (= random1 random2 random3))))

;;; test-stopwatch
(test test-stopwatch
      (let ((sleep nil)
            (final nil))
        (with-stopwatch ()
          (sleep 1)
          (setf sleep (apr::sw-delta))
          (apr::sw-reset)
          (sleep 1)
          (setf final (apr::sw-delta)))
        (is (and (= 1 sleep)
                 (= 1 final)))))

;;; test-with-kernel
;;; RP  Thu Mar 14 22:20:00 2024
(test test-with-kernel
      (let ((res1 0)
            (res2 0))
        (with-kernel ()
          (do-frames (i 10)
            (incf res1)))
        (with-kernel (:stopwatch? nil)
          (do-frames (i 10)
            (incf res2)))
        (is (and (= res1 10)
                 (= res2 10)))))

;;; test-sc-1
#+slippery-chicken
(test test-sc-1
      (let ((a (frame-start (sc::make-event 'c4 'q :start-time 2.0)
                            :frame-rate 25))
            (b (frame-end (sc::make-event 'c4 'q :start-time 0.0)
                          :frame-rate 25))
            (c (frame-duration (sc::make-event 'c4 'q. :start-time 2.0)
                               :frame-rate 25)))
        (is (and (= a 50)
                 (= b 25)
                 (= c 38)))))

;;; test-sc-2
#+slippery-chicken
(test test-sc-2
      (let* ((rthms '(32 16 s. te fs te fe 32 32 64 64))
             ;;(rthms '(q 32 q s. s. s e te te fe te q))
             (pitches '(c4 d4))
             (num-events 10)
             (tempo 120.0)
             (events (sc:events-update-time
                      (loop repeat num-events
                            for i from 0
                            with pt-len = (length pitches)
                            with rt-len = (length rthms)
                            collect
                            ;; sorry for the randomness
                            (sc:make-event (nth (sc:random-rep pt-len (zerop i))
                                                pitches)
                                           (nth (sc:random-rep rt-len)
                                                rthms)))
                      :tempo tempo))
             (dur-secs (sc:end-time (car (last events))))
             (dur-frames (secs->frames dur-secs))
             (out-width 1920)
             (out-height 1080)
             ;; blueprint
             (ps (make-projection-surface :x-scaler 1
                                          :y-scaler 1
                                          :width out-width
                                          :height out-height
                                          :color '(255 255 255 255)))
             (frame-counter 1)
             (src-vids `((c4 ,(make-image-file-seq-from-video
                               (test-pathname "test1.mp4")))
                         (d4 ,(make-image-file-seq-from-video
                               (test-pathname "test2.mp4")))))
             (outdir "/tmp/apr-sc-test-2/"))
        (ensure-directories-exist outdir)
        (with-kernel ()
          (do-frames (i dur-frames)
            (with-stopwatch ()
              (let ((tmp-ps (clone ps))
                    (outfile (format nil "~a~4,'0d.jpg" outdir i)))
                (loop for e in events
                      for count from 0
                      do
                         (timeline ((frames->secs i) (sc:start-time e)
                                    :duration (sc:duration e)
                                    :tl-time-acc tl-time
                                    :tl-duration-acc tl-duration)
                                   (let* ((start-offset (* 13 count))
                                          (ifs (second
                                                (find (sc:data
                                                       (sc:pitch-or-chord e))
                                                      src-vids :key #'car)))
                                          ;; always (re-)start video from
                                          ;; from first frame and loop
                                          (ifs-dur-f
                                            (secs->frames (duration ifs)))
                                          (vid-i
                                            (1+ (mod (+
                                                      (- i (secs->frames
                                                            (sc:start-time e)))
                                                           start-offset)
                                                          ifs-dur-f)))
                                          (tmp-img (get-image ifs vid-i))
                                          (tmp-pn
                                            (make-projection tmp-img
                                                             :projection-height
                                                             out-height
                                                             :projection-width
                                                             out-width)))
                                     (compose tmp-ps tmp-pn))))
                (write-jpg tmp-ps :outfile outfile)
                (format t "File: ~a~%~
                     Frame: ~a/~a~%~
                     Duration: ~a sec~%"
                        outfile
                        frame-counter dur-frames
                        (apr::sw-delta))
                (incf frame-counter)))))
        (format t "Converting to video. ~%")
        (image-seq->video outdir (format nil "~avid.mp4" outdir)
                          :glob-pattern "*.jpg")
        (is (and (probe-file (format nil "~a0001.jpg" outdir))
                 (probe-file (format nil "~avid.mp4" outdir))))))

;;; test-svg-files->png
(test test-svg-files->png
      (let ((indir (test-pathname "svg-seq/"))
            (outdir "/tmp/apr-test-batch/"))
        (apr::svg-files->png :indir indir
                             :outdir outdir)
        (is (probe-file (concatenate 'string
                                     outdir
                                     "1.png")))))

;;; test-improvisation-1
;;; RP  Mon Mar 25 23:22:37 2024
(test test-improvisation-1
      (let* ((infile1 (test-pathname "composer.jpg"))
             (infile2 (test-pathname "composers.jpg"))
             (projection1 (make-projection infile1 :projection-height 50))
             (projection2 (make-projection infile2 :projection-height 50))
             (ps (make-projection-surface :surface-width 200
                                          :surface-height 100
                                          :x-scaler 1/10
                                          :y-scaler 1/10
                                          :color '(255 255 255 0)))
             (outfile "/tmp/test-impr.png"))
        (compose ps projection1 :dest-x 100 :dest-y 20
                                :width 25)
        (compose ps projection1 :dest-x 125 :dest-y 45
                                :width 25 :src-x 25
                                :height 25 :src-y 25)
        (compose ps (scale (clone projection1) .5 .5))
        (compose ps projection1 :dest-y 50)
        ;; change the opacity
        (imago::do-image-pixels ((data projection2) color x y)
          (setf color (imago::make-color (imago::color-red color)
                                         (imago::color-green color)
                                         (imago::color-blue color)
                                         200)))
        (compose ps (rotate (clone projection2) 80) :dest-y 15)
        (compose ps projection2 :dest-x 145 :dest-y 65
                                :width 20 :height 15
                                :src-x 5 :src-y 8)
        (write-png ps :outfile outfile)
        (is (probe-file outfile))))

;;; test-compositing
;;; RP  Wed Mar 27 19:17:08 2024
(test test-compositing
      (let* ((img1 (make-rgb-image
                    200 200
                    :initial-color (imago::make-color 30 65 90 255)))
             (img2 (make-rgb-image
                    200 200
                    :initial-color (imago::make-color 34 199 200 255)))
             (outfile "/tmp/test-")
             (outfiles '()))
        (loop for operator in '(a-over-b-fun
                                a-in-b-fun
                                a-out-b-fun
                                a-xor-b-fun
                                a-atop-b-fun)
              for out = (concatenate 'string
                                     outfile
                                     (write-to-string operator)
                                     ".png")
              for cv = (make-canvas 500 500 :color '(0 0 0 0))
              do
                 (compose cv img1 :compose-fun #'a-over-b-fun
                                  :dest-x 0 :dest-y 0)
                 (compose cv img2 :compose-fun (symbol-function operator)
                                  :dest-x 150 :dest-y 150
                                  :width 100 :height 100
                                  :complete? t)
                 (push out outfiles)
                 (write-png cv :outfile out))
        (is (every #'pathnamep (mapcar #'probe-file outfiles)))))

;;; test-timeline1
(test test-timeline1
      (let ((res (loop for i from 1 to 10
                       collect
                       (timeline (i 3 :end 8 )
                                 apr::tl-time))))
        (is (equal '(0 1 2 3 4 5)
                   (remove nil res)))))

;;; test-sc1
#+slippery-chicken
(test test-sc1
      (let* ((num-seqs 11)
             (proc (sc:procession num-seqs '(1 2 3 4)))
             (hello 
               (sc:make-slippery-chicken
                '+hello-slippery+ 
                :composer "ME"
                :title "hello slippery chicken noodles"
                :year "2024"
                :tempo-map '((1 (q 144 "fast")))
                :ensemble '(((vln (violin :midi-channel 1))
                             (perc (vibraphone :midi-channel 4))
                             (bsn (bassoon :midi-channel 9))))
                :rthm-seq-palette '((1 ((((5 8) - s x 4 -  - s e s - (e)))
                                        :pitch-seq-palette ((1 2 5 4 1 3 6)
                                                            (5 3 2 6 4 1 2))))
                                    (2 ((((5 8) - s s s - (s)  (e) - s x 4 -))
                                        :pitch-seq-palette ((1 3 2 6 4 2 3))))
                                    (3 ((((5 8) (e) - e s s - - s e s -))
                                        :pitch-seq-palette ((1 4 2 3 6 3))))
                                    (4 ((((5 8) - s e s -  e (e) - s s -))
                                        :pitch-seq-palette ((4 1 2 5 2 3)))))
                :set-palette '((1 ((d2 f2 ef3 b3 g4 af4 a4 bf5)))
                               (2 ((b1 e2 d3 bf3 f4 g4 a4 fs5)))
                               (3 ((ef2 a2 d3 b2 a3 f4 c5 fs5 bf5)))
                               (4 ((cs2 bf2 fs3 e4 c5 g5 d6))))
                :set-limits-high '((vln (0 c5 75 c7 100 c5))
                                   (perc (0 a4 75 f6 100 a4))
                                   (bsn (0 g3 75 a4 100 g3)))
                :set-limits-low '((vln (0 c4 75 g5 100 c4))
                                  (perc (0 f3 75 g4 100 f3))
                                  (bsn (0 bf1 75 g3 100 bf1)))
                :set-map `((1 ,proc))
                :rthm-seq-map `((1 ((vln ,proc)
                                    (perc ,(sc::wrap-list proc 4))
                                    (bsn ,(sc::wrap-list proc 7)))))))
             (sc-events (sc:get-events-sorted-by-time hello))
             (sc-data
               (mapcar #'(lambda (e)
                           (when (sc:pitch-or-chord e)
                             (list (sc:start-time e)
                                   (sc:duration e)
                                   (sc::midi-note
                                    (if (sc:is-chord e)
                                        (first (sc:data (sc:pitch-or-chord e)))
                                        (sc:pitch-or-chord e)))
                                   (sc:player e))))
                       sc-events))
             (colors '((vln . (10 29 190 255))
                       (perc . (20 200 28 255))
                       (bsn . (100 10 29 255))))
             (dur (sc:duration (sc:piece hello)))
             (dur-frames (+ (secs->frames dur) (get-apr-config :fps))) 
             (out-width 900)
             (out-height 400)
             (surface-width 90)
             (surface-height 40)
             ;; the projection-surface (which serves as a "canvas")
             (ps (make-projection-surface :surface-width surface-width
                                          :surface-height surface-height
                                          :width out-width
                                          :height out-height
                                          :color '(255 255 255 255)))
             (frame-counter 1)
             (outdir "/tmp/sc-seq/"))
        (ensure-directories-exist outdir)
        (with-kernel ()
          (do-frames (i dur-frames)
            (with-stopwatch ()
              (let ((tmp-ps (clone ps))
                    (outfile (format nil "~a~4,'0d.jpg" outdir i)))
                (loop for e in sc-data
                      when (second e)
                        do
                           (timeline ((frames->secs i) (first e)
                                      :duration (second e)
                                      :tl-time-acc tl-time
                                      :tl-duration-acc tl-duration)
                                     (let* ((color (alexandria:assoc-value
                                                    colors (fourth e)))
                                            (tmp-img (make-rgb-image
                                                      20 20
                                                      :initial-color
                                                      (imago::make-color
                                                       ;; grayscale
                                                       (first color)
                                                       (second color)
                                                       (third color)
                                                       (+ 100
                                                          (random 155)))))
                                            (tmp-pn (make-projection
                                                     tmp-img
                                                     :projection-height .8))
                                            (pn-x
                                              (cm::rescale (third e)
                                                           0 128 0 90)) 
                                            (pn-y (list 1 30)))
                                       ;; put the tmp-img onto the
                                       ;; projection-surface
                                       (compose tmp-ps tmp-pn
                                                :dest-x pn-x
                                                :dest-y (interpolate-easing
                                                         tl-time
                                                         (first pn-y)
                                                         (second pn-y)
                                                         :duration
                                                         tl-duration
                                                         :ease-fun
                                                         #'ease:out-bounce)))))
                (write-jpg tmp-ps :outfile outfile)
                (format t "File: ~a~%~
                     Frame: ~a/~a~%~
                     Duration: ~a sec~%"
                        outfile
                        frame-counter dur-frames
                        (apr::sw-delta))
                (incf frame-counter)))))
        (is (probe-file (concatenate 'string
                                     outdir
                                     "0000.jpg")))))

;;; test-image-file-seq-1
(test test-image-file-seq-1
      (let* ((ifs (make-image-file-seq-from-video (test-pathname "test1.mp4"))))
        (is (typep (get-image ifs 2) 'image))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
