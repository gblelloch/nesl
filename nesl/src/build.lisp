;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(when (not (find-package 'nesl-lisp))
  (make-package 'nesl-lisp))

(in-package :nesl-lisp)

;;;;;;;;;;;;;;;;;
;; The toplevel Nesl directory.
;;;;;;;;;;;;;;;;
(defparameter *NESL-PATH* 
  (if (boundp 'user::*nesl-path*)
      user::*nesl-path*
    (namestring *default-pathname-defaults*)))

(defparameter *srcdir* "neslsrc/")
(defparameter *bindir* "neslsrc/")

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
;;;; COMPILE --- THIS CAN BE REMOVED AFTER THE FILES HAVE BEEN COMPILED
;;;;;;;;;;;;;;

(setq *compile-print* nil)
(load (fullname "make-nesl.lisp"))

;;;;;;;;;;;;;;
;;;; LOAD
;;;;;;;;;;;;;;

(load (fullname "load-nesl.lisp"))

(format t "
You have just compiled and loaded NESL.   
(1) To start NESL type:
      (nesl)
(2) If you want to create an executable dump of nesl:
      A: type \"(nesl)\" to start up NESL.
      B: type \"dump world;\" to dump an executable to nesl/bin/runnesl,
         or \"dump world <filename>;\" to dump it to another file.
      C: type \"exit;\" to exit NESL and lisp.
    The dump will require at least 10 Megabytes of memory and can require
    up to 35 Megabytes for Lucid Common Lisp.
(3) If you don't want to create an executable dump, but would rather load
    NESL each time you start up, then you should modify the variable
    \"*nesl-path*\" in the file \"neslsrc/load.lisp\".   You can
    then start NESL by starting Common Lisp and loading the file 
    load.lisp.
")

