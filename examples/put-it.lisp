;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; put-it.lisp
;;;
;;; NAME
;;; put-it
;;;
;;; DESCRIPTION
;;; This file demonstrates how to use the projection-surface and projection
;;; classes. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-25
;;;
;;; $$ Last modified:  16:56:48 Mon Mar 25 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;; display some additional information
(set-apr-config :verbose t)

(let* ((infile (path-from-same-dir "composer.jpg"))
       ;; This object creates a "projection" from the image file and sets the
       ;; projection-height of the image to 50 (and implicitly sets the width to
       ;; a value that matches the proportions of the source image, in this case
       ;; 50, as the image is square). The projection-height and -width
       ;; determine the dimensions relative to the apparence projection context,
       ;; which facilitates working with source and destination images of
       ;; different resolutions by unifying the dimensions to a common scale.
       ;; Internally, there are x- and y-scalers which contain information about
       ;; the translation of the apparence-scale to the actual pixel-values of
       ;; the image file (cf. projection).
       (projection (make-projection infile :projection-height 50))
       ;; This is the projection-surface, an abstraction of a canvas. Its
       ;; purpose is to contain one or more projections and translate the
       ;; image data to an output canvas object with fixed pixel-dimensions
       ;; determined by the scalers or the width- and height-attributes (cf.
       ;; projection-surface). The surface-width and -height follow the same
       ;; principle ("apparence-scale/-unit") as described above. The color
       ;; determines the default (background-)color of the projection-surface. 
       (ps (make-projection-surface :surface-width 200
                                    :surface-height 100
                                    :x-scaler 1/10
                                    :y-scaler 1/10
                                    :color '(255 255 255 255))))
  ;; This places a part of the projection object onto the projection-surface.
  (put-it ps projection :dest-x 100 :dest-y 20
                        :width 25)
  ;; ...and this another part. 
  (put-it ps projection :dest-x 125 :dest-y 45
                        :width 25 :src-x 25
                        :height 25 :src-y 25)
  ;; This writes the projection-surface to a jpeg-file and opens it in the
  ;; default program for viewing jpeg-files.
  (system-open-file (write-jpg ps)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF put-it.lisp
