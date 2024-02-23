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
;;; $$ Last modified:  18:13:08 Fri Feb 23 2024 CET
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
