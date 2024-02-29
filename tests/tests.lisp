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
;;; $$ Last modified:  14:41:17 Thu Feb 29 2024 CET
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
(test test-circular-projection
  (let* ((img (apparence::make-rgb-image 500 400
                                         (apparence::make-color 233 200 188)))
         (result
           (apparence::circular-projection img 350 0)))
    (imago::write-png result "/tmp/test.png"))
  (is (probe-file "/tmp/test.png")))

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
        (img (apparence::make-rgb-image 50 100
                                        (apparence::make-color 100 233 90))))
    (imago::copy (data cv) img)
    (write-png cv :outfile "/tmp/test-canvas.png"))
  (is (probe-file "/tmp/test-canvas.png")))

;;; test-put-it-simple
;;; RP  Thu Feb 29 14:02:15 2024
(test test-put-it-simple
  (let* ((cv (apr:make-canvas 300 200 :color '(0 0 0 0)))
         (img (apr::make-rgb-image 50 100 (apr::make-color 100 233 90))))
    (apr:put-it cv img :dest-x 20)
    (apr:write-png cv :outfile "/tmp/cv-test.png"))
  (is (probe-file "/tmp/cv-test.png")))

;;; test projection surface
;;; RP  Mon Feb 26 17:49:28 2024
(test test-projection-surface
  (let ((ps (make-projection-surface 10 20.5)))
    (is (= (width ps) 10.0))))

;;; test cylinder-mantle
;;; RP  Mon Feb 26 17:53:24 2024
(test test-cylinder-mantle
  (let ((mantle1 (make-cylinder-mantle 10
                                       :width 2.0))
        (mantle2 (make-cylinder-mantle 5.5
                                       :diameter 3.0)))
    (setf (diameter mantle1) 3.0)
    (is (= (width mantle1) (width mantle2)))))

;;; test-make-canvas-from-ps
;;; RP  Thu Feb 29 14:37:40 2024
(test test-make-canvas-from-ps
  (let* ((ps (apr:make-projection-surface 200 300))
         (cv (apr:make-canvas-from-ps ps :factor 2.0)))
    (is (= (width cv) 400))))

;;; test-make-canvas-from-cm
;;; RP  Thu Feb 29 14:39:50 2024
(test test-make-canvas-from-cm
  (let* ((cm (apr:make-cylinder-mantle 200 :diameter 30.0))
         (cv (apr:make-canvas-from-ps cm :destination-height 3000)))
    (is (= (width cv) 1413))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
