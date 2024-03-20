;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* apr/export-symbols
;;; NAME
;;; export-symbols
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-02-26
;;; 
;;; PURPOSE
;;; This module exports all symbols from the apparence package. 
;;;
;;;
;;; $$ Last modified:  17:01:47 Wed Mar 20 2024 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :apparence)


(let ((package (find-package :apparence)))
  (do-all-symbols (symb package)
    (when (and (or (find-class symb nil)
                   (fboundp symb))
               (eql (symbol-package symb) package))
      (export symb package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF export-symbols.lisp
