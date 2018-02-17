;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(when (not (find-package 'nesl-lisp))
  (make-package 'nesl-lisp))

(in-package :nesl-lisp)

(defparameter *nesl-version* "Sequential 1.0")

(defparameter *dump-lisp-version* nil 
  "To preserve the intermediate lisp version in the nesl -> fasl translation")

(defparameter *nesl-date* "June 18, 1995")

(defparameter *obj-tailer* 
  #+(and (or allegro excl) (not allegro-version>=)) ".faslold"
  #+allegro-version>= ".fasl"
  #+lucid ".sbin"
  #+cmu ".f"
  #+clisp ".fsl"
  #+kcl ".o")


#+allegro (setf (mp:global-symbol-value 'excl:*redefinition-warnings*) nil)
#+lucid (setq *redefinition-action* :quiet)

;; only compiles a file if the source is newer
(defun compile-and-load (filename)
  (let ((src-file (concatenate 'string filename ".lisp"))
	(obj-file (concatenate 'string filename *obj-tailer*)))
    (when (or (not (probe-file obj-file))
	      (< (file-write-date obj-file)
		 (file-write-date src-file)))
      (compile-file src-file :output-file obj-file))
    (load obj-file)))

(defun compile-and-load-nesl (filename)
  (let ((src-file (concatenate 'string filename ".nesl"))
	(obj-file (concatenate 'string filename *obj-tailer*))
	(lsp-file (concatenate 'string filename ".nlisp")))
    (when (or (not (probe-file obj-file))
	      (< (file-write-date obj-file)
		 (file-write-date src-file)))
	  (convert-to-lisp src-file lsp-file)
	  (compile-file lsp-file :output-file obj-file))
    (load obj-file)))

(setq *compile-verbose* nil)
(setq *compile-print* nil)


(let ((*error-output* nil))
  (load "load2.lisp"))
