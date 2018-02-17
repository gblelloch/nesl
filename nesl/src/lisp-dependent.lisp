;; This file contains functions whose implementation depends on the
;; particular common-lisp being used.
;; All other files are strictly by the book.
(in-package :nesl-lisp)

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
  #-(or lucid allegro excl cmu kcl clisp)
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
  #+allegro (let ((fname (concatenate 'string filename ".dxl")))
	      (declare (special excl:*restart-app-function*))
	      (setq excl:*restart-app-function* restart-function)
	      (format t "
Saving as ~a.  
You need to run mlisp -I ~a to restart."
		      fname fname)
              (excl::dumplisp :name fname :suppress-allegro-cl-banner t))
  ;; Note that KCL does not take a restart function argument
  #+kcl (progn
	  (setq si::*top-level-hook* restart-function)
	  (si::save-system filename))
  #+lucid (disksave filename :restart-function restart-function)
  #+clisp (ext:saveinitmem filename :executable t 
			   :init-function restart-function)
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
  #+allegro (user::exit 0 :quiet t)
  #+kcl (user::bye)
  #+lucid (user::quit)
  #+cmu (lisp::quit)
  #+clisp (user::quit))

(defmacro nesl-ignore-errors (body) 
  ;; Old versions of Lisp do not support ignore-errors
  #+(or (and (or allegro excl) (not allegro-version>=))
	kcl)
  `,body
  #-(or (and (or allegro excl) (not allegro-version>=))
	kcl)
  `(ignore-errors ,body))
