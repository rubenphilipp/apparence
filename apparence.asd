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
;;; $$ Last modified:  17:59:44 Mon Feb 26 2024 CET
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
               (:file "projection")
               ;; export needs to be done lastly
               (:file "export")))


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
;;; Export all symbols
;;; RP  Mon Feb 26 17:56:11 2024

;; (let ((package (find-package :apparence)))
;;   (do-all-symbols (symb package)
;;     (when (and (or (find-class symb nil)
;;                    (fboundp symb))
;;                (eql (symbol-package symb) package))
;;       (export symb package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF apparence.asd
