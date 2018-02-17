;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

;; The package nesl-lisp needs to be created and
;; the variables *nesl-path*, *nesl-version*, *nesl-date*, *srcdir* 
;; and *bindir* need to be set before this file can be loaded.

(in-package :nesl-lisp)

(format t "~%; Loading NESL version ~a, ~a~%~%"
	*nesl-version* *nesl-date*)

(when (not (boundp '*obj-tailer*))
  (defparameter *obj-tailer* 
    #+(and (or allegro excl) (not allegro-version>=)) ".faslold"
    #+allegro-version>= ".fasl"
    #+lucid ".sbin"
    #+cmu ".f"
    #+clisp ".fas"
    #+kcl ".o"))

#+allegro-version>= (excl:set-case-mode :case-insensitive-upper)

(let ((*default-pathname-defaults* (pathname *nesl-path*)))
  (declare (special *default-pathname-defaults* *argument_check* 
		    *fully-loaded* *definitions*))
  (catch 'nesl-error (progn)
    (flet ((load-bin-file (filename)
	     (when (not (load (concatenate 'string *nesl-path* *bindir* 
					   filename *obj-tailer*)
			      :if-does-not-exist nil))
	       (format t "Sorry, there are no compiled versions of ~a.~%"
		       filename)))
	   (load-nesl-file (filename)
	     (load-nesl (concatenate 'string *nesl-path* *srcdir* filename))))
       (load-bin-file "lisp-dependent")
       (load-bin-file "funspec")
       (load-bin-file "free")
       (load-bin-file "trans")
       (load-bin-file "ptrans")
       (load-bin-file "type")
       (load-bin-file "type-check")
       (load-bin-file "strip-recs")
       (load-bin-file "output")
       (load-bin-file "run-vcode")
       (load-bin-file "vcode-lisp")
       (load-bin-file "defop")
       (load-bin-file "nesl-read")
       (load-bin-file "read-eval-print")
       (load-bin-file "pprint")
       (load-bin-file "trace")
       (load-bin-file "doc")
       (load-bin-file "tokens")
       (load-bin-file "parse")

       ;; Hack because of bug in kcl.
       #+kcl
       (defun ctyi () (read-char standard-input nil -1))

       (let ((*redefine-default* 'off))
	 (declare (special *redefine-default*))
	 (load-nesl-file "types.lnesl")
	 (load-nesl-file "scalar-ops.lnesl")
	 (load-nesl-file "vector-ops.lnesl")
	 (load-nesl-file "nest-ops.lnesl")
	 (load-nesl-file "io.lnesl")
	 (load-nesl-file "plot.nesl")
	 (load-nesl-file "ltoc.lnesl")
	 (load-nesl-file "other-ops.nesl")
	 (load-nesl-file "window.nesl")
	 (load-nesl "config.nesl")
	 )

       (setq *argument_check* t)
       (checkall *definitions*)
       (setq *fully-loaded* t)
       )))
