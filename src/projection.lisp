;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; projection.lisp
;;;
;;; NAME
;;; projection
;;;
;;; DESCRIPTION
;;; Implementation of different approaches to project/map images on various
;;; surfaces/planes. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;;
;;; $$ Last modified:  19:30:39 Fri Feb 23 2024 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* projection/circular-projection
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; DESCRIPTION This function projects an image onto a circularly folded canvas
;;; (i.e. the lateral surface / mantle of a cylinder). The circular
;;; position/azimuth of the source image can be defined by a degree offset from
;;; the center-origin on the canvas (which is relative to the width of the
;;; unfolded, plane mantle).
;;;
;;; ARGUMENTS
;;; - The image to be projected, as an imago image object.
;;; - The azimuth degree for the projection.
;;; - The vertical position of the image relative to the top of the image. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :canvas-width. The width of the canvas (i.e. the mantle) in px.
;;;   Default = 4000
;;; - :canvas-height. The height of the canvas/mantle in px.
;;;   Default = 2000
;;; - :canvas-origin. The horizontal position of the origin (i.e. the center) on
;;;   the canvas. This value is relative to the width of the canvas and must be
;;;   a number between 0.0 and 1.0.  Default = 0.0
;;; - :image-origin. The horizontal position of the origin on the image to be
;;;   projected. This value is relative to the image width and must be a number
;;;   between 0.0 and 1.0.  Default = 0.5
;;; - :canvas-color. The rgb(a)-background color of the canvas.
;;;   Default = (make-color 255 255 255 0)
;;; 
;;; 
;;; RETURN VALUE
;;; An image object. 
;;;
;;; EXAMPLE
#|
(let* ((img (make-rgb-image 500 400 (make-color 233 200 188)))
       (result
         (circular-projection img 350 0)))
  (write-png result "/tmp/test.png")
  (system-open-file "/tmp/test.png"))
|#
;;; SYNOPSIS
(defun circular-projection (image azimuth y
                            &key
                              (canvas-width 4000)
                              (canvas-height 2000)
                              (canvas-origin 0.0)
                              (image-origin 0.5)
                              (canvas-color (make-color 255 255 255 0)))
  ;;; ****
  ;;; sanity checks
  (unless (and (<= 0.0 image-origin) (>= 1.0 image-origin))
    (error "The image-origin must >= 0.0 and <= 1.0"))
  (unless (and (<= 0.0 canvas-origin) (>= 1.0 canvas-origin))
    (error "The canvas-origin must >= 0.0 and <= 1.0"))
  (when (>= y canvas-height)
    (error "circular-projection: The y-coordinate is >= the canvas-height."))
  (let* ((canvas (make-rgb-image canvas-width
                                 canvas-height
                                 canvas-color))
         (img-width (image-width image))
         ;;(img-height (image-height image))
         ;; anchor point (x-axis from left) on image, i.e. the origin x-coords
         (img-anchor (floor (* img-width image-origin)))
         (rotation-angle (mod azimuth 360))
         ;; the absolute canvas origin
         (canvas-anchor (floor (* canvas-origin canvas-width)))
         ;; rotation origin on the canvas
         (rotation-origin (floor (+ (* (/ canvas-width 360)
                                       rotation-angle)
                                    canvas-anchor)))
         ;; the x-coordinates for the image on the canvas (x-left x-right)
         (image-x-coords (list
                             ;; left
                             (- rotation-origin img-anchor)
                             ;; right
                             (+ rotation-origin
                                (- img-width img-anchor)))))
    ;; project the image onto the cylindrical plane / mantle
    ;; in case there is an overlap on either the left or right side of the
    ;; canvas, wrap the image
    (cond
      ((and (<= 0 (first image-x-coords))
            (>= canvas-width (second image-x-coords)))
       (copy canvas image :dest-x (first image-x-coords)
                          :dest-y y))
      ((and (> 0 (first image-x-coords))
            (>= canvas-width (second image-x-coords)))
       (print "left->right")
       ;; wrap left->right
       ;; starting with the "left" part (i.e. the right part of the img)
       (copy canvas image :width (second image-x-coords)
                          :src-x (abs (first image-x-coords))
                          :dest-y y)
       ;; now the "right" part
       (copy canvas image :width (abs (first image-x-coords))
                          :src-x 0
                          :dest-x (+ canvas-width
                                     (first image-x-coords))
                          :dest-y y))
      ((and (<= 0 (first image-x-coords))
            (< canvas-width (second image-x-coords)))
       (print "right->left")
       ;; wrap right->left
       ;; starting with the right part (the left part of the img)
       (copy canvas image :width (- canvas-width
                                    (first image-x-coords))
                          :src-x 0
                          :dest-x (first image-x-coords)
                          :dest-y y)
       ;; now the left part
       (copy canvas image :width (- (second image-x-coords)
                                    canvas-width)
                          :src-x (- canvas-width
                                    (first image-x-coords))
                          :dest-x 0 :dest-y y))
      (t (error "the width of the image to be projected is greater than ~
                 the width of the projection screen.")))
    canvas))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF projection.lisp
