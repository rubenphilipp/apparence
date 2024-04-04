;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; globals.lisp
;;;
;;; NAME
;;; globals
;;;
;;; DESCRIPTION
;;; Definition of global configuration data for apparence. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;;
;;; $$ Last modified:  20:53:03 Thu Apr  4 2024 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* globals/*apparence-config-data*
;;; NAME
;;; *apparence-config-data*
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; DESCRIPTION
;;; A global holding information about the configuration of apparence. 
;;; 
(defparameter *apparence-config-data*
  `(;; the default fps
    (:fps . 30)
    ;; default interpolation mode
    (:default-interpolation . :nearest-neighbor)
    ;; the command for to the inkscape binary
    (:inkscape-command . "/opt/homebrew/bin/inkscape")
    ;; a seed for the pcg generator (see random.lisp)
    (:pcg-seed . 4294967296)
    ;; the default tmp directory (trailing slash!)
    (:default-tmp-dir . "/tmp/apparence/")
    ;; the default rgb-color
    (:default-rgb . ,(make-color 0 0 0 0))
    ;; verbose mode?
    (:verbose . nil)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* globals/get-apr-config
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; DESCRIPTION
;;; Returns the value of a configuration setting from the global
;;; *apparence-config-data*.
;;;
;;; SYNOPSIS
(defun get-apr-config (key)
  ;;; ****
  (declare (special *apparence-config-data*))
  (assoc-value *apparence-config-data* key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* globals/set-apr-config
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-23
;;; 
;;; DESCRIPTION
;;; Set the value of a element in the *apparence-config-data* global.
;;;
;;; ARGUMENTS
;;; - The key to the element in *apparence-config-data*.
;;; - The new value.
;;; 
;;; RETURN VALUE
;;; The new value of the config element.
;;; 
;;; SYNOPSIS
(defun set-apr-config (key value)
  ;;; ****
  (declare (special *apparence-config-data*))
  (setf (assoc-value *apparence-config-data* key) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter cl-user::+apparence-src-path+
  (namestring (asdf::system-relative-pathname "apparence" "src/")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF globals.lisp
