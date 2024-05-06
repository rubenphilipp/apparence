;;; apr-mode.el --- a minor mode for apparence      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ruben Philipp

;; Author: Ruben Philipp <me@rubenphilipp.com>
;; Keywords: lisp, extensions, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the apr-mode minor mode to Emacs.  It is aimed
;; at working with the Common Lisp software apparence.
;; This file stores mode-specific bindings to `apr-mode`, and minor-mode
;; definition.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* emacs/apr-mode
;;; NAME
;;; apr-mode
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-05-06
;;; 
;;; PURPOSE
;;; Emacs minor mode for apparence.
;;;
;;;
;;; $$ Last modified:  21:49:07 Mon May  6 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sly)
(require 'cl-lib)
(require 'apr-plot)

;;;###autoload
(define-minor-mode apr-mode
  "Toggle apr-mode."
  :lighter "apparence"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c p e") 'apr-plot-envelope)
            map))


(provide 'apr-mode)
;;; apr-mode.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF apr-mode.el
