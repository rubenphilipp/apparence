;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apr/timeline
;;; NAME
;;; timeline
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-30
;;; 
;;; PURPOSE
;;; Implementation of some timeline related functions and macros. 
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined. 
;;;
;;; $$ Last modified:  19:33:02 Sun Mar 31 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun with-timeline-aux (time start end duration
                          &optional
                            (fps (get-apr-config :fps))
                            (round-fun #'round))
  ;;; ****
  (when (and end duration)
    (error "timeline::with-timeline-aux: You can't set both end and duration ~
            of a timeline."))
  (when (or (not start) (not (or end duration)))
    (error "timeline::with-timeline-aux: You have to set a value for start as ~
            well as for end or duration."))
  (when (and end (<= end start))
    (error "timeline::with-timeline-aux: The end of the timeline must be ~
            greater than the start."))
  (if end
      (setf duration (- end start))
      (setf end (+ start duration)))
  (let* ((abs-frame (secs->frames time :frame-rate fps
                                       :round-fun round-fun))
         (rel-sec (- time start))
         (rel-frame (secs->frames rel-sec :frame-rate fps
                                          :round-fun round-fun)))
    (values rel-sec rel-frame abs-frame duration end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** timeline/with-timeline
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-03-30
;;; 
;;; DESCRIPTION
;;; This macro establishes a local timeline context. The body form will only be
;;; evaluated when the time is within the time frame as indicated by the start
;;; and end, resp. start and duration arguments. All time values are given in
;;; seconds (as a float). Within the form, several local values are available,
;;; such as the relative and absolute time values of the current time point
;;; on the given timeline. Relative means relative to the start time of the
;;; timeline, while absolute means relative to 0. The variable accessor symbols
;;; can be set via the given -acc keywords.
;;;
;;; ARGUMENTS
;;; - The current time stamp in the  global sequence (i.e. the "playhead"
;;;   location), in seconds. 
;;; - The start time of the timelinie in the sequence (in seconds).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :end. The time of the timeline (in seconds). Don't use this argument when
;;;   a :duration is specified. 
;;; - :duration. The duration of the timeline (in seconds). Don't use this
;;;   argument when :end is specified. 
;;; - :tl-frame-acc. The accessor symbol for the local variable holding the
;;;   frame index relative to the start of the timeline, available within the
;;;   body. Default = 'tl-frame
;;; - :tl-time-acc. The accessor symbol for the local variable holding the time
;;;   (in seconds) relative to the start of the timeline, available within the
;;;   body. Default = 'tl-time
;;; - :tl-abs-frame-acc. The accessor symbol for the local variable holding the
;;;   absolute frame index (relative to 0), available within the body.
;;;   Default = 'tl-abs-frame
;;; - :tl-abs-time-acc. The accessor symbol for the local variable holding the
;;;   absolute time (in seconds; relative to 0), available within the body.
;;;   Default = 'tl-abs-time
;;; 
;;; BODY
;;; 
;;; One or many forms which are evaluated only when the time (i.e. the
;;; "playhead") is >= start and <= end.
;;;
;;; RETURN VALUE
;;; If the body form is evaluated (see above), the result(s) from the evaluation
;;; of the body form. Otherwise NIL. 
;;; 
;;;
;;; EXAMPLE
#|
(let* ((seq-length 3.5)
       (seq-frames (secs->frames seq-length)))
  (remove nil
          (loop for i from 1 to seq-frames
                collect
                (with-timeline ((frames->secs i) 1.0 :duration 1.0)
                  tl-time))))

;; =>
(0.0 0.03999996 0.08000004 0.120000005 0.15999997 0.20000005 0.24000001
 0.27999997 0.32000005 0.36 0.39999998 0.44000006 0.48000002 0.52 0.55999994
 0.6 0.64 0.67999995 0.72 0.76 0.79999995 0.84000003 0.88 0.91999996 0.96000004
 1.0)
|#
;;; SYNOPSIS
(defmacro with-timeline ((time start
                          &key
                            ;; never set both end and duration
                            end
                            duration
                            ;; relative to timeline start
                            (tl-frame-acc 'tl-frame)
                            (tl-time-acc 'tl-time)
                            ;; absolute to the timeline context
                            (tl-abs-frame-acc 'tl-abs-frame)
                            (tl-abs-time-acc 'tl-abs-time)
                            (round-fun #'round)
                            (fps (get-apr-config :fps)))
                         &body body)
  ;;; ****
  `(with-gensyms (duration end)
     (multiple-value-bind (,tl-time-acc ,tl-frame-acc ,tl-abs-frame-acc
                           duration end)
         (with-timeline-aux ,time ,start ,end ,duration ,fps ,round-fun)
       (let ((,tl-abs-time-acc ,time))
         (when (and (<= ,start ,time) (>= end ,time))
           ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF timeline.lisp
