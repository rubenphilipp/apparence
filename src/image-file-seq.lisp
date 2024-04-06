;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* apr/image-file-seq
;;; NAME
;;; image-file-seq
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-05
;;; 
;;; PURPOSE
;;; Implementation of the image-file-seq class. This class holds information on
;;; a sequence of image-files, most likely sequential images used as frames of
;;; a video. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> seq -> image-file-seq
;;;
;;; $$ Last modified:  15:17:57 Sat Apr  6 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

(defclass image-file-seq (seq)
  ((fps :accessor fps :initarg :fps :initform nil)
   ;; duration of the seq in seconds
   (duration :accessor duration :initarg :duration :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; not much to do for now
;;; RP  Sat Apr  6 00:24:53 2024
(defmethod initialize-instance :after ((ifs image-file-seq) &rest initargs)
  (declare (ignore initargs))
  (update ifs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((ifs image-file-seq) stream)
  (format stream "~%IMAGE-FILE-SEQ: fps: ~a, duration: ~a"
          (fps ifs) (duration ifs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((ifs image-file-seq))
  (clone-with-new-class ifs 'image-file-seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((ifs image-file-seq) new-class)
  (declare (ignore new-class))
  (let ((nc (call-next-method)))
    (setf (slot-value nc 'fps) (fps ifs)
          (slot-value nc 'duration) (duration ifs))
    nc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update :after ((ifs image-file-seq) &key ignore)
  (declare (ignore ignore))
  (setf (duration ifs) (frames->secs (length (data ifs))
                                     :frame-rate (fps ifs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* image-file-seq/make-image-file-seq-from-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-06
;;; 
;;; DESCRIPTION
;;; This function instantiates an image-file-seq object from a given directory,
;;; assuming it contains numerically named image files which constitute an
;;; image sequence.
;;; The data slot will hold the pathnames to the image files, with an id derived
;;; from the numerical filename (e.g. 0021.png will result in the id 21). 
;;;
;;; ARGUMENTS
;;; The path to the directory containing the image files. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the image-file-seq
;;; - :fps. The frame rate related to the image file sequence in the directory.
;;;   This value will also be used to derive the duration (in secs) of the
;;;   image-file-seq. Default = (get-apr-config :fps)
;;; - :pattern. The search pattern used to determine files to be included in the
;;;   image-file-seq. Default = "*.png"
;;; 
;;; RETURN VALUE
;;; The new image-file-seq object. 
;;;
;;; EXAMPLE
#|
(let ((ifs (make-image-file-seq-from-dir "/tmp/tl-seq/" :pattern "*.jpg")))
  (get-item ifs 0))
;; => #P"/tmp/tl-seq/0000.jpg"
|#
;;; SYNOPSIS
(defun make-image-file-seq-from-dir (path &key
                                      id
                                      (fps (get-apr-config :fps))
                                      (pattern "*.png"))
  ;;; ****
  (setf path (trailing-slash path))
  (let* ((files (uiop:directory-files path pattern))
         (alist
           (list->alist (loop for file in files
                              collect
                              (list
                               (handler-case
                                   (parse-integer (pathname-name file))
                                 (error ()
                                   (error "image-file-seq::~
                                           make-image-file-seq-from-dir: The ~
                                           filenames are not named ~
                                           numerically.")))
                               file))
                        :unique t :sort t)))
    (make-instance 'image-file-seq :data alist :id id :fps fps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* image-file-seq/make-image-file-seq-from-video
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-06
;;; 
;;; DESCRIPTION
;;; This function instantiates an image-file-seq object from a video file. By
;;; doing so, it extracts one image for each frame via ffmpeg and stores them
;;; in the :image-files-dir or, when this argument is omitted, in the
;;; default-tmp-dir.
;;; Optionally, a :start time for the conversion can be defined, as well as a
;;; number of frames to extract (:num-frames, starting from :start).
;;;
;;; ARGUMENTS
;;; The path to the video file. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :image-files-dir. The directory where the image files retrieved from the
;;;   video file will be stored. When NIL, a path relative to the
;;;   (get-apr-config :default-tmp-dir) will be generated.
;;; - :id. The id of the image-file-seq object.
;;; - :pattern. The search pattern used to determine files to be included in the
;;;   image-file-seq. NB: This should be "in sync" with the nomenclature/suffix
;;;   indicated by the :outfile-pattern. Default = "*.png"
;;; - :outfile-pattern. The filename pattern used for generating names for the
;;;   output image files. Default = "%04d.png"
;;; - :fps. The frame rate used to extract images from the video file.
;;;   Default = (get-apr-config :fps)
;;; - :start. The datum in the video file to start image extraction, in seconds.
;;;   Default = 0
;;; - :num-frames. An integer indicating the number of frames to extract from
;;;   the :start datum. When NIL, the video will be processed until the end.
;;; - :verbose. Print more information on the process.
;;;   Default = (get-apr-config :verbose)
;;; 
;;; RETURN VALUE
;;; The new image-file-seq object. 
;;;
;;; EXAMPLE
#|
(let ((ifs (make-image-file-seq-from-video "/tmp/test.mp4" :fps 30)))
  (get-frame ifs 20))
;; => #P"/tmp/apparence/f36a9a14-4873-4f23-8ee7-f9b9860e43cf/0020.png"

;; NOTE: There is also a "shortcut" to this function:

(video->image-file-seq "/tmp/test.mp4" :fps 1)

;; =>
IMAGE-FILE-SEQ: fps: 1, duration: 5
SEQ: 
NAMED-OBJECT: id: NIL, tag: NIL, 
data: ((1 . /tmp/apparence/1057a8ef-5579-4080-a3df-89dd74dba05d/0001.png)
       (2 . /tmp/apparence/1057a8ef-5579-4080-a3df-89dd74dba05d/0002.png)
       (3 . /tmp/apparence/1057a8ef-5579-4080-a3df-89dd74dba05d/0003.png)
       (4 . /tmp/apparence/1057a8ef-5579-4080-a3df-89dd74dba05d/0004.png)
       (5 . /tmp/apparence/1057a8ef-5579-4080-a3df-89dd74dba05d/0005.png))
|#
;;; SYNOPSIS
(defun make-image-file-seq-from-video (vidfile
                                       &key
                                         ;; when no value is given, the images
                                         ;; will stored in a directory relative
                                         ;; to the default-tmp-dir
                                         image-files-dir
                                         id
                                         (pattern "*.png")
                                         ;; inherited from video->image-seq:
                                         (outfile-pattern "%04d.png")
                                         (fps (get-apr-config :fps))
                                         (start 0)
                                         num-frames
                                         (verbose (get-apr-config :verbose)))
  ;;; ****
  (unless image-files-dir
    (setf image-files-dir (concatenate 'string
                                       (get-apr-config :default-tmp-dir)
                                       (get-random-uuid)
                                       "/"))
    (when verbose
      (warn "image-file-seq::make-image-file-seq-from-video: No ~
             image-files-dir is given. Putting the results of the convsersion ~
             to the tmp-dir.")))
  (setf image-files-dir (trailing-slash image-files-dir))
  (video->image-seq vidfile image-files-dir :outfile-pattern outfile-pattern
                                            :fps fps
                                            :start start
                                            :num-frames num-frames
                                            :verbose verbose)
  (make-image-file-seq-from-dir image-files-dir
                                :id id
                                :fps fps
                                :pattern pattern))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortcut:
#|
(video->image-file-seq "/tmp/test.mp4" :fps 1)
|#
(defun video->image-file-seq (&rest args)
  (apply #'make-image-file-seq-from-video args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* image-file-seq/get-frame
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-06
;;; 
;;; DESCRIPTION
;;; This method returns the image file (as a pathname) for a secified frame in
;;; an image-file-seq. 
;;;
;;; ARGUMENTS
;;; - An image-file-seq object.
;;; - The datum of the frame to retrieve, in frames (integer) or seconds (when
;;;   :in-seconds is T).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :in-seconds. When T, the unit of datum will be interpreted as seconds,
;;;   otherwise it will be treated as a frame index.
;;; - :warn? When T, prints additional warnings (e.g. when the frame does not
;;;   exist in the image-file-seq).
;;; 
;;; RETURN VALUE
;;; The pathname of the respective image file. 
;;;
;;; EXAMPLE
#|
(let ((ifs (make-image-file-seq-from-dir "/tmp/tl-seq/" :pattern "*.jpg")))
  (get-frame ifs 2 :in-seconds t))
;; => #P"/tmp/tl-seq/0060.jpg"
|#
;;; SYNOPSIS
(defmethod get-frame ((ifs image-file-seq) datum
                      &key
                        ;; when T, the datum is interpreted as a value in
                        ;; seconds relative to the fps of the image-file-seq
                        ;; otherwise it is an absolute frame
                        in-seconds
                        warn?)
  ;;; ****
  (when in-seconds
    (setf datum (secs->frames datum :frame-rate (fps ifs))))
  (get-item ifs datum :warn? warn?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* image-file-seq/get-image
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-04-06
;;; 
;;; DESCRIPTION
;;; Like get-frame, this method tries to retrieve an image file from an
;;; image-file-seq at a given datum/index. Instead of returning the pathname of
;;; this file, it will instantiate and return an image object.
;;;
;;; ARGUMENTS
;;; - An image-file-seq object.
;;; - The datum of the frame to retrieve, in frames (integer) or seconds (when
;;;   :in-seconds is T).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the image object.
;;; - :default-interpolation. The default interpolation method (used e.g. when
;;;   changing the image dimensions via the (setf ...) methods.
;;;   Default = (get-apr-config :default-interpolation)
;;; - :in-seconds. When T, the unit of datum will be interpreted as seconds,
;;;   otherwise it will be treated as a frame index.
;;; - :warn? When T, prints additional warnings (e.g. when the frame does not
;;;   exist in the image-file-seq).
;;; 
;;; RETURN VALUE
;;; 
;;;
;;; EXAMPLE
#|
(let ((ifs (make-image-file-seq-from-dir "/tmp/tl-seq/" :pattern "*.jpg")))
  (get-image ifs 1 :in-seconds t))
;; =>
IMAGE: width: 500, height: 300
NAMED-OBJECT: id: NIL, tag: NIL, 
data: #<RGB-IMAGE (500x300) {7008616523}>
**********
|#
;;; SYNOPSIS
(defmethod get-image ((ifs image-file-seq) datum
                      &key
                        id
                        (default-interpolation
                         (get-apr-config :default-interpolation))
                        in-seconds
                        warn?)
  ;;; ****
  (let* ((path (get-frame ifs datum :warn? warn? :in-seconds in-seconds))
         (image
           (make-image-from-file path
                                 :id id
                                 :default-interpolation default-interpolation)))
    image))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF image-file-seq.lisp
