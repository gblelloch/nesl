;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

;; This file contains functions whose implementation depends on the
;; particular common-lisp being used.
;; All other files are strictly by the book.
(in-package :nesl-lisp)


#+gcl 
(progn
  (setq compiler:*compile-verbose* nil)
  (setq compiler:*compile-print* nil)
  (setq compiler:*suppress-compiler-notes* t) 
  (setq compiler:*suppress-compiler-warnings* t)
)



;;; This takes a string and executes it as if it was a line of text
;;; to the shell.  Standard in, out and error go to the terminal.
(defun run-shell-line (command-string)
  #+lucid
  (lcl::shell command-string)
  #+(or allegro excl)
  (system::run-shell-command command-string)
  #+cmu
  (extensions:run-program "/bin/csh" (list "-fc" command-string)
			  :output *standard-output* :error *standard-output*)
  #+clisp
  (system::shell command-string)
  #+kcl
  (system command-string)
  #-(or lucid allegro excl cmu kcl)
  (error "The NESL/VCODE interface has not been implemented ~
          for the Lisp you are using."))

(defun getenv (varname)
  "Gets an environment variable.  
  Returns a string if defined or nil if undefined."
  #+cmu (cdr (assoc (intern varname "KEYWORD") extensions::*environment-list*))
  #+(and lucid lcl3.0) (lucid-common-lisp:environment-variable varname)
  #+(and lucid (not lcl3.0))  (system:environment-variable varname)
  #+(or allegro excl) (system:getenv varname)
  #+clisp (system::getenv varname)
  #+kcl (si:getenv varname)
  #-(or allegro lucid cmu excl kcl clisp) nil)

(defun save-lisp-image (filename restart-function)
  #+allegro (excl::dumplisp :name filename :restart-function restart-function)
  ;; Note that KCL does not take a restart function argument
  #+kcl (progn
	  (setq si::*top-level-hook* restart-function)
	  (si::save-system filename))
  #+lucid (disksave filename :restart-function restart-function)
  #+clisp (nesl-error t "Sorry dumping is not supported in CLISP.
If you know how to save a CLISP image, please send us a note.")
  #+cmu (progn
	  (format t "
Warning: the lisp image of CMU Common Lisp requires over 20 Megabytes.
To restart a CMU Common Lisp core image, you need to execute:
  lisp -core <filename>
")
	  (if restart-function
	      (extensions:save-lisp filename :purify t 
				    :init-function restart-function)
	    (extensions:save-lisp filename :purify t)))
  )

(defun quit-lisp ()
  #+allegro (user::exit)
  #+kcl (user::bye)
  #+lucid (user::quit)
  #+cmu (user::quit)
  #+clisp (user::quit))

(defmacro nesl-ignore-errors (body) 
  ;; Old versions of Lisp do not support ignore-errors
  #+(or (and (or allegro excl) (not allegro-version>=))
	kcl)
  `,body
  #-(or (and (or allegro excl) (not allegro-version>=))
	kcl)
  `(ignore-errors ,body))
