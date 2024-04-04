;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apr/slippery-chicken
;;; NAME
;;; slippery-chicken
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-21
;;; 
;;; PURPOSE
;;; This module implements some helper methods for slippery-chicken objects.
;;;
;;;
;;; $$ Last modified:  20:58:54 Thu Apr  4 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(let ((e (sc::make-event 'c4 'q :start-time 2.0)))
  (frame-start e :frame-rate 25))
;; => 50
|#
(defmethod frame-start ((event sc::event)
                        &key
                        (round-fun #'round)
                        (frame-rate (get-apr-config :fps)))
  ;;; ****
  (secs->frames (sc::start-time event)
                :round-fun round-fun
                :frame-rate frame-rate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(let ((e (sc::make-event 'c4 'q :start-time 0.0)))
  (frame-end e :frame-rate 25))
;; => 25
|#
(defmethod frame-end ((event sc::event)
                      &key
                        (round-fun #'round)
                        (frame-rate (get-apr-config :fps)))
  ;;; ****
  (let ((start (frame-start event :round-fun round-fun
                                  :frame-rate frame-rate)))
    (+ start (secs->frames (sc::duration event)
                           :round-fun round-fun
                           :frame-rate frame-rate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(let ((e (sc::make-event 'c4 'q. :start-time 2.0)))
  (frame-duration e :frame-rate 25))
;; => 38
|#
(defmethod frame-duration ((event sc::event)
                           &key
                             (round-fun #'round)
                             (frame-rate (get-apr-config :fps)))
  ;;; ****
  (secs->frames (sc::duration event)
                :round-fun round-fun
                :frame-rate frame-rate))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF slippery-chicken.lisp
