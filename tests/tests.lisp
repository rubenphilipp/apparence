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
;;; $$ Last modified:  23:40:28 Sat Mar  2 2024 CET
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

;;; test circular projection
;;; RP  Fri Feb 23 19:31:49 2024
;;; removed (deprecated)
;;; RP  Thu Feb 29 20:42:50 2024
#|
(test test-circular-projection
  (let* ((img (imago::make-rgb-image 500 400
                                     (imago::make-color 233 200 188)))
         (result
           (apparence::circular-projection img 350 0)))
    (imago::write-png result "/tmp/test.png"))
(is (probe-file "/tmp/test.png")))
|#

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

;;; test-put-it-simple
;;; RP  Thu Feb 29 14:02:15 2024
(test test-put-it-simple
  (let* ((cv (apr:make-canvas 300 200 :color '(0 0 0 0)))
         (img (apr::make-rgb-image 50 100
                                   :initial-color
                                   (imago::make-color 100 233 90))))
    (apr:put-it cv img :dest-x 20)
    (apr:write-png cv :outfile "/tmp/cv-test.png"))
  (is (probe-file "/tmp/cv-test.png")))

#|
;;; test cylinder-mantle
;;; RP  Mon Feb 26 17:53:24 2024
(test test-cylinder-mantle
  (let ((mantle1 (make-cylinder-mantle 10
                                       :width 2.0))
        (mantle2 (make-cylinder-mantle 5.5
                                       :diameter 3.0)))
    (setf (diameter mantle1) 3.0)
    (is (= (width mantle1) (width mantle2)))))
|#

#|
;;; test-make-canvas-from-ps
;;; RP  Thu Feb 29 14:37:40 2024
(test test-make-canvas-from-ps
  (let* ((ps (apr:make-projection-surface 200 300 1.0 1.0))
         (cv (apr:make-canvas-from-ps ps :factor 2.0)))
(is (= (width cv) 400))))
|#

#|
;;; test-make-canvas-from-cm
;;; RP  Thu Feb 29 14:39:50 2024
(test test-make-canvas-from-cm
  (let* ((cm (apr:make-cylinder-mantle 200 :diameter 30.0))
         (cv (apr:make-canvas-from-ps cm :destination-height 3000)))
(is (= (width cv) 1413))))
|#

;;; test-get-coordinates
;;; RP  Thu Feb 29 14:42:06 2024
(test test-get-coordinates
  (let* ((cm (apr:make-cylinder-mantle 10 :diameter 10))
         (coords (apr:get-coordinates cm 90 :azimuth-origin-offset 0)))
    (is (equal coords '(5.0d0 0.0d0)))))

;;; test-put-it-circular
;;; RP  Thu Feb 29 15:42:13 2024
(test test-put-it-circular
  (let* ((cv (apr:make-canvas 1000 500 :color '(0 0 0 0)))
         (img (apr:make-rgb-image 250 200
                                  :initial-color
                                  (apr::make-color 233 200 188))))
    (apr:put-it-circular cv img 350 0)
    (is (typep (data cv) 'apr:image))))

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
  (let ((ps (apr:make-projection-surface 20 30.5 10.5 10.5)))
    (is (typep ps 'apr:projection-surface))))

;;; test-ps-setters1
;;; RP  Fri Mar  1 23:12:14 2024
(test test-ps-setters1
  (let ((ps (apr:make-projection-surface 20 30 10.5 10.5)))
    (setf (width ps) 40)
    (setf (surface-width ps) 10)
    (setf (y-scaler ps) 4)
    (setf (x-scaler ps) 10)
    (setf (height ps) 50)
    (setf (width ps) 250)
    (is (and (= 10 (surface-width ps))
             (= 30 (surface-height ps))
             (= 25 (x-scaler ps))
             (= 5/3 (y-scaler ps))
             (= 250 (width (data ps)))
             (= 50 (height (data ps)))))))

;;; test-projection-simple1
;;; RP  Sat Mar  2 18:07:57 2024
(test test-projection-simple1
  (let ((pn (make-instance 'projection :projection-width 400
                                     :projection-height 600
                                     :x-scaler 2
                                     :y-scaler 2
                                     :data (imago::make-rgb-image 200 300))))
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
                               :initial-color (imago::make-color 210 180 90)))
         (img2 (make-rgb-image 100 100
                             :initial-color (imago::make-color 120 120 90)))
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
                            :initial-color (imago::make-color 210 180 90)))
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
                              :initial-color (imago::make-color 210 180 90)))
         (pn (make-projection img
                              :x-scaler 2)))
    (scale pn 2 2)
    (is (and (= (projection-width pn) 800)
             (= (x-scaler pn) 4)
             (= (projection-height pn) 1600)
             (= (y-scaler pn) 4)))))


;;; test-put-it-ps-pn
;;; RP  Sat Mar  2 23:37:50 2024
(test test-put-it-ps-pn
      (let* ((cm (make-projection-surface (* 38 pi) 40.36670443321064d0
                                          (/ 8192 (* 38.0 pi))
                                          (/ 2770 40.36670443321064d0)))
             (img-orig (make-rgb-image 2000 1381 :initial-color
                                       (imago::make-color 222 100 189)))
             (pn (make-projection img-orig
                                  :projection-height 40.36670443321064d0)))
        (put-it cm pn :src-y 0 :dest-y 37.5 :dest-x 100.15)
        (when (probe-file "/tmp/put-it-ps.jpg")
          (delete-file "/tmp/put-it-ps.jpg"))
        (write-jpg cm :outfile "/tmp/put-it-ps.jpg")
        (is (probe-file "/tmp/put-it-ps.jpg"))))
              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
