;;; apr-plot.el --- a minor mode for apparence  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* emacs/apr-mode/apr-plot
;;; NAME
;;; apr-plot
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-05-06
;;; 
;;; PURPOSE
;;; This module implements plotting capabilities for the apr-mode. 
;;;
;;; $$ Last modified:  21:48:23 Mon May  6 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sly)
;;#+slime (require 'slime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* apr-mode/apr-plot-envelope
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-05-06
;;; 
;;; DESCRIPTION
;;; Reads the last expression from a buffer and evaluates apr::plot-envelope,
;;; assuming the expression is a proper list, in order to plot the values of
;;; the list as an envelope. 
;;;
;;; ARGUMENTS
;;; none
;;; 
;;; OPTIONAL ARGUMENTS
;;; A list to be tested. Must be provided as a string. 
;;; 
;;; RETURN VALUE
;;; The return value from the evaluation of the lisp form. 
;;;
;;; EXAMPLE
[
 (apr-plot-envelope "'(0 30 45 70 100 89)")
]
;;; SYNOPSIS
(defun apr-plot-envelope (&optional env-list)
;;; ****
  "Reads the last expression from a buffer and calls apr:plot-envelope"
  (interactive)
  (let* ((env-list (if env-list
                       env-list
                     (sly-last-expression)))
         (cl-expr (concat "(apr::plot-envelope "
                          env-list
                          ")")))
    (sly-eval `(cl:eval
                (cl:read-from-string ,cl-expr))
              "apparence")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'apr-plot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF apr-plot.el
