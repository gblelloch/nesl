;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

;; The package nesl-lisp needs to be created and
;; the variables *nesl-path*, *srcdir* and *bindir* 
;; need to be set before this file can be loaded.

(in-package :nesl-lisp)

(proclaim '(optimize (compilation-speed 3)
		     (speed 3)))

(defparameter *obj-tailer* 
  #+(and (or allegro excl) (not allegro-version>=)) ".faslold"
  #+allegro-version>= ".fasl"
  #+clisp ".fas"
  #+lucid ".sbin"
  #+cmu ".f"
  #+kcl ".o")

#+allegro-version>= (excl:set-case-mode :case-insensitive-upper)

(let ((src-dir (concatenate 'string *nesl-path* *srcdir*))
      (obj-dir (concatenate 'string *nesl-path* *bindir*)))
  (flet ((comp-lisp-file (name &key needs)
	   (let ((src-file (concatenate 'string src-dir name ".lisp"))
		 (obj-file (concatenate 'string obj-dir name *obj-tailer*)))
	     (when (not (probe-file src-file))
	       (error "No source file ~a." src-file))
	     (when (or (not (probe-file obj-file))
		       (< (file-write-date obj-file)
			  (file-write-date src-file)))
	       (when needs (load needs))
	       (compile-file src-file :output-file obj-file))))
	 (load-file (name)
	    (load (concatenate 'string obj-dir name *obj-tailer*))))
    (comp-lisp-file "funspec")
    (load-file "funspec")
    (comp-lisp-file "lisp-dependent")
    (load-file "lisp-dependent")
    (comp-lisp-file "vcode-lisp")
    (comp-lisp-file "type")
    (comp-lisp-file "pprint")
    (comp-lisp-file "free")
    (comp-lisp-file "trans")
    (comp-lisp-file "ptrans")
    (comp-lisp-file "type-check")
    (comp-lisp-file "trace")
    (comp-lisp-file "strip-recs")
    (comp-lisp-file "output")
    (comp-lisp-file "run-vcode")
    (comp-lisp-file "tokens")
    (comp-lisp-file "parse")
    (comp-lisp-file "defop")
    (comp-lisp-file "nesl-read")
    (comp-lisp-file "doc")
    (comp-lisp-file "read-eval-print")
    ))
