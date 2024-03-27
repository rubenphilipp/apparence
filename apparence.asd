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
;;; $$ Last modified:  14:08:46 Wed Mar 27 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "apparence"
  :description "Common Lisp library for working with (moving) images."
  :version "0.0.1"
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :serial t 
  :in-order-to ((test-op (test-op "apparence/tests")))
  :depends-on ("alexandria"
               "cl-ppcre"
               ;; this is necessary, as re-loading the cm-version that
               ;; comes with slippery-chicken might lead to naming conflicts
               ;; RP  Tue Mar 12 16:17:06 2024
               (:feature (:not :slippery-chicken) "cm")
               "parse-float"
               "serapeum"
               "cl-svg"
               "lparallel"
               "vgplot"
               "frugal-uuid/non-frugal"
               "cl-pcg"
               "imago"
               "imago/pngload"
               "imago/jpeg-turbo")
  :pathname "src/"
  :components ((:file "package")
               (:file "named-object")
               (:file "parallel")
               (:file "utilities")
               (:file "globals")
               (:file "random")
               (:file "slippery-chicken" :if-feature :slippery-chicken)
               (:file "svg")
               (:file "compositing")
               (:file "imago")
               (:file "image")
               (:file "canvas")
               (:file "projection")
               (:file "projection-surface")
               (:file "cylinder-mantle")
               ;; export needs to be done lastly
               (:file "export-symbols")))


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

(in-package :cl-user)

(defun apr (&optional (logo t))
  (declare (special +apparence-src-path+))
  (setf *package* (find-package :apparence))
  (when logo
    (let* ((apr-logo (concatenate 'string +apparence-src-path+
                                 "txt/apr-ascii-logo-small.txt"))
           (in (open apr-logo :if-does-not-exist nil)))
      (when in
        (loop for line = (read-line in nil)
              while line do (format t "~&~a" line))
        (close in))))
  +sbcl t
  +-sbcl (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF apparence.asd
