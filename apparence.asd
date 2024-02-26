;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* system
;;; NAME
;;; system
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; PURPOSE
;;; System definition for apparence. 
;;;
;;;
;;; $$ Last modified:  15:33:56 Mon Feb 26 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "apparence"
  :description "Common Lisp library for working with (moving) images."
  :version "0.0.1"
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :serial nil ;; could also be T; TODO: test/elaborate
  :in-order-to ((test-op (test-op "apparence/tests")))
  :depends-on ("alexandria"
               "cl-ppcre"
               "imago"
               "imago/pngload"
               "imago/jpeg-turbo")
  :pathname "src/"
  :components ((:file "package")
               (:file "named-object")
               (:file "utilities")
               (:file "globals")
               (:file "projection")))


;;; regression tests
(defsystem "apparence/tests"
  :description "Test suite for apparence."
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :depends-on ("apparence"
               "fiveam")
  :pathname "tests/"
  :perform (test-op (o c) (symbol-call :apparence.tests :run-tests))
  :components ((:file "tests")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF apparence.asd
