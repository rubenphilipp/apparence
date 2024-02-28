;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apr/package
;;; NAME
;;; package
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; PURPOSE
;;; Package definition for apparence. 
;;;
;;;
;;; $$ Last modified:  23:01:44 Wed Feb 28 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :apparence
  (:use :common-lisp)
  (:nicknames :apr)
  (:import-from
   :imago
   :image
   :make-color
   :image-width
   :image-height
   :color-red
   :color-green
   :color-blue
   :color-alpha
   :resize
   :scale
   :copy
   :make-rgb-image
   )
  (:import-from
   :cm
   :rescale
   :rescale-envelope
   :interp
   :interpl)
  (:import-from
   :alexandria
   :assoc-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lisp
