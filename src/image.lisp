;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/image
;;; NAME
;;; image
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-29
;;; 
;;; PURPOSE
;;; Implementation of the image class. This class holds in its data-slot
;;; an imago::image and provides an interface for accessing relevant attributes.
;;;
;;; NB: This abstraction of the imago::image class is necessary in order to
;;;     provide convenient facilities of referencing images (e.g. to computing
;;;     sdf ressources when working with a large amount of images). 
;;;
;;; CLASS HIERARCHY
;;; named-object -> image
;;;
;;; $$ Last modified:  20:28:47 Thu Feb 29 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF image.lisp
