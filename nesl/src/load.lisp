;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

;;;;;;;;;;;;;;;;
;; This file is like build.lisp, but it assumes all the lisp files
;; have been compiled.  It also requires that you set the variable 
;; *NESL-PATH*.
;;;;;;;;;;;;;;;;

(when (not (find-package 'nesl-lisp))
  (make-package 'nesl-lisp))

(in-package :nesl-lisp)

;;;;;;;;;;;;;;;;;
;; ********************************************************
;; *nesl-path* NEEDS TO BE SET TO THE LOCATION OF THE NESL DIRECTORY.
;; EVERYTHING ELSE IN THE FILE CAN BE LEFT AS IS.
;; ********************************************************
;;;;;;;;;;;;;;;;

(defparameter *nesl-path* "/usr/foo/nesl/")

(defparameter *srcdir* "neslsrc/") ;; relative to *nesl-path*
(defparameter *bindir* "neslsrc/") ;; relative to *nesl-path*

;;;;;;;;;;;;;;
;;;; SET VERSION NUMBER
;;;;;;;;;;;;;;

(defparameter *nesl-version* nil)

(defparameter *nesl-date* nil)

(defun fullname (name) (concatenate 'string *NESL-PATH* *srcdir* name))

(with-open-file (releasefile (fullname "releasenum") :direction :input)
  (setq *nesl-version* (read-line releasefile))
  (setq *nesl-date* (read-line releasefile)))

;;;;;;;;;;;;;;
;;;; LOAD
;;;;;;;;;;;;;;

(load (fullname "load-nesl.lisp"))

(when (and (boundp *fully-loaded*) *fully-loaded*)
  (format t "

; --> To start NESL type: (nesl) <--

"))
