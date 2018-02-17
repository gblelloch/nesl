;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(compile-and-load "lisp-dependent")
(compile-and-load "structs")
(compile-and-load "other-stuff")
(compile-and-load "tokens")
(compile-and-load "parse")
(compile-and-load "pprint")
(compile-and-load "type-check")
(compile-and-load "lisp-parse")
(compile-and-load "free")
(compile-and-load "interp")
(compile-and-load "read-eval-print")
(compile-and-load "init-func")


(setq *load-verbose* nil)
(setq *load-print* nil)


;;; ADD THE PRIMITIVE FUNCTIONS

(compile-and-load "primitive-functions")

;(add-primitive 't t '(bool) 
;	       '(:documentation "The boolean value TRUE.")) 
(add-primitive 'f 'nil '(bool) 
	       '(:documentation "The boolean value FALSE.")) 
(add-primitive 'pi 3.141592653589793d0 '(float) 
	       '(:documentation "The value of $pi$."))
(add-primitive 'max_int *max_int* '(int) nil)
(add-primitive 'min_int *min_int* '(int) nil)

(defparameter *ready* nil)

(format t "Loading Scalars...~%")
(setq *defs* (add-other-functions *defs* "scalar-ops"))

(format t "Loading Vectors...~%")
(setq *defs* (add-other-functions *defs* "vector-ops"))

(format t "Loading I/O...~%")
(setq *defs* (add-other-functions *defs* "io-ops"))

;(format t "Loading Other Functions...~%")
;(setq *defs* (nload-nesl "other-ops.nesl" *defs* :verbose nil))


(setq *original-defs* *defs*)









