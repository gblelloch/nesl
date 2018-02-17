;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

(defparameter *prompt* "[Nesl] ")
(defparameter *defs* nil)
(defparameter *nesl-server* nil)
(defparameter *step-count* 0)

(defparameter *special-forms* 
  '(|,| function datatype =))

(defparameter *show-commands*
  '(bugs config configs code status))

(defparameter *set-commands*
  '(trace profile print_length arg_check verbose debug redefine
    lnesl config memory_size rsh_command editor))

(defparameter *show-commands-assoc*
  '((bugs . 0) (config . 0) (configs . 0) 
    (code . 1) (status . 0)))

(defparameter *set-commands-assoc*
  '((trace . -1) (profile . -1) 
    (print_length . 1) (arg_check . 1)
    (verbose . 1) (debug . 1) (redefine . 1)
    (lnesl . 1) (config . 1)
    (memory_size . 1) (rsh_command . 1) 
    (editor . 1)))


(defparameter *nesl-bugs* `(
"Certain errors will drop you out of the read/eval/print loop into
the lisp error handler.   You can restart by typing (nesl)."

"Timings cannot be nested."

"Spawn and the graphics routines are not currently supported."
))


;;;;;;;;;;;;
;;; HELP
;;;;;;;;;;;;

(defparameter *help-string*
"
NESL top-level forms:
  function <name> <pattern> [: <typespec>] = <exp>;  -- Function Definition
  datatype <name> <typeexp> [:: <typebind>];         -- Record Definition
  <pattern> = <exp>;                                 -- Top level Assignment
  <exp>;                                             -- Any NESL expression

Top-level Commands:
(Only the commands available in the sequential version are listed below)

  help;                    -- Print this message.
  describe <funcname>;     -- Describe a NESL function.
  apropos <name>;          -- List functions which contain <name>.
  set print_length <n>;    -- Set maximum sequence print length.
  show code <funcname>;    -- Show the code for a function.
  load <exp>;              -- Load a file.
  obj_load <exp>;          -- Load the binary version of the file. 
  show bugs;               -- List the known bugs.
  dump world [<filename>]; -- Dump current NESL environment to a file.
  dump info [<filename>];  -- Dump info for bug reports (default=stdout).
  exit; or ^D;             -- exit nesl.
")

(defun print-error-message ()
  (format t 
      "~%This command is currently not supported in the sequential version."))

(defun nesl-help ()
  (format t *help-string*))


(defun nesl-list-bugs ()
  (let ((i 0))
    (dolist (bug *nesl-bugs*)
      (format t "~%~a: ~a~%" (incf i) bug))))


(defun describe-nesl (form definitions)
  (declare (special *special-forms* *set-commands* *show-commands* *doclist*))
  (when (not (= 1 (length form)))
    (nesl-error 
     "Bad syntax for describe, should be in form: describe <fname>;"))
  (let* ((funname (car form))
	 (binding (find-def funname definitions)))
    (cond 
     ((eql funname 'trace)
	   (format t "~%Syntax: set trace <funname> <level>;~%~%~
             Trace a NESL function.~%  ~
               level 0 = no tracing~%  ~
               level 1 = show function entry and exit~%  ~
               level 2 = show argument and result values at entry and exit~%  ~
               level 3 = show entry, exit, and each top level let binding~%  ~
               level 4 = combination of levels 2 and 3~%"))
     ((member funname *special-forms*)
      (format t "~%~a is a special form; please see the NESL manual." 
	      funname))
     ((or (member funname *set-commands*)
	  (member funname *show-commands*))
      (format t "~%~a is a top level command; please use (help)." 
	      funname))
     (binding 
      (let* ((fundef (binding-original-code binding))
	     (interface (cons (func-name fundef)
			      (func-arguments fundef)))
	     (type (func-type fundef))
	     (type (if type type (binding-type binding)))
	     (funcp (func-arguments fundef))
	     (code (func-body fundef))
	     (serial? (get-keyword :serial (func-other fundef) nil))
	     (prim? (get-keyword :primitive (func-other fundef) nil))
	     (doc (get-keyword :documentation (func-other fundef) nil)))
	(when funcp
	      (format t "~%INTERFACE:~% ~a~%"  
		      (pprint-nesl-string interface)))
	(format t (if serial? "~%TYPE: (callable in serial only)~%~% ~a~%"
		    "~%TYPE:~%~% ~a~%")
		(pprint-oneline-string (cnesl-full-type type)))
	(when doc  (format t "~%DOCUMENTATION:~%~% ~a~%" doc))
	(when funcp
	      (if prim?
		  (format t "~%CODE: *Primitive*~%")
	        (let ((lines (cnesl-exp code)))
		  (format t "~%CODE:~%~a~%" (pprint-nesl-string-rec lines 2 nil
								    6))
		  (when (> (length lines) 6)
			(format 
			 t "~%Use \"show code ~(~a~);\" to see all the code."
			 funname)))))))
     (t (format t "~a not found." funname)))
    definitions))



(defun show-code (funname definitions)
  (let* ((binding (find-def funname definitions)))
    (if binding
	(let ((fundef (binding-original-code binding)))
	  (if (or (binding-datatype binding) (func-arguments fundef))
	      (let ((code (func-body fundef)))
		(if (get-keyword :primitive (func-other fundef) nil)
		    (format t "~%~a is a primitive." funname)
		  (let* ((interface (cons (func-name fundef)
					  (func-arguments fundef)))
			 (type (func-type fundef))
			 (type (if type type (binding-type binding))))
		    (format t "~%CODE:~%~a : ~a = ~a~%" 
			    (pprint-nesl-string interface)
			    (pprint-oneline-string (cnesl-full-type type))
			    (pprint-nesl-string-rec (cnesl-exp code) 
						    2 nil -1)))))
	    (format t "~%~a is not a function definition." funname)))
      (format t "~%Function ~a not found." funname))))


(defun eval-show (command definitions)
  (let ((com (cdr (assoc (car command) *show-commands-assoc*))))
    (when (not com)
      (nesl-error "Unknown top level command: SHOW ~a" (car command)))
    (when (not (= (length (cdr command)) com))
      (nesl-error "Wrong number of arguments to \"show ~(~a~)\""(car command)))
    (cond ((eql (car command) 'bugs) (nesl-list-bugs))
	  ((eql (car command) 'code)
	   (show-code (second command) definitions))
	  (t (print-error-message))))
  definitions)


(defun eval-set (command definitions)
  (declare (special *max-print-length*))
  (let ((com (cdr (assoc (car command) *set-commands-assoc*))))
    (when (not com)
      (nesl-error "Unknown top level command: SET ~a" (car command)))
    (when (and (not (minusp com)) (not (= (length (cdr command)) com)))
      (nesl-error "Wrong number of arguments to \"set ~(~a~)\"" (car command)))
    (cond ((eql (car command) 'print_length)
	   (when (not (integerp (second command)))
	     (nesl-error 
	      "The argument to SET PRINT_LENGTH must be an integer."))
	   (setq *max-print-length* (second command)))
	  (t (print-error-message))))
  definitions)


;;;;;;;;;;;;
;;; STUFF FOR LOADING FILES
;;;;;;;;;;;;

(defun load-nesl (filename defns &key (verbose t) (print nil))
  (when (or (not (stringp filename)) (zerop (length filename)))
    (nesl-error 
     "The second argument to LOAD must be a nonempty character string."))
  (let* ((filename (namestring (merge-pathnames filename)))
	 (filename (cond ((probe-file filename))
			 ((probe-file (concatenate 'string filename ".nesl")))
			 (t (nesl-error "File ~a does not exist." filename)))))
    (with-open-file (loadstr filename :direction :input)
		    (when (and verbose (not (eql verbose 'nil)))
			  (format t "~%% Loading ~a. %" filename))
	  (nesl-loop loadstr defns nil nil :interactive nil :print print))))


(defun nload-nesl (file defns &key (verbose t) (print nil))
  (declare (ignore print))
  (when (or (not (stringp file)) (zerop (length file)))
    (nesl-error 
     "The second argument to LOAD must be a nonempty character string."))
  (let* ((filename (namestring (merge-pathnames file)))
	 (filename (cond ((probe-file filename))
			 ((probe-file (concatenate 'string filename ".nesl")))
			 (t (nesl-error "File ~a does not exist." filename)))))
    (when (and verbose (not (eql verbose 'nil)))
	  (format t "~%% Loading ~a. %" filename))
    (nload-nesl-nocheck filename defns)))


(defun nload-nesl-nocheck (filename defns &key (verbose t) (print nil))
  (declare (ignore verbose))
  (declare (special *load-error* *curr-lisp-code* *curr-checklist* 
		    *curr-lisp-code*))
  (let* ((pre-filename (reverse (subseq (reverse (namestring filename)) 5)))
	 (lsp-filename (concatenate 'string pre-filename ".nlisp"))
	 (obj-filename (concatenate 'string pre-filename *obj-tailer*)))
    (setq *load-error* nil)
    (if (and (probe-file obj-filename)
	     (> (file-write-date obj-filename) (file-write-date filename)))
	(progn 
	  (catch 'nload-error 
	    (let ((*error-output* nil)) (load obj-filename)))
	  (if *load-error* 
	      (progn (delete-file obj-filename)
		     (nload-nesl-nocheck filename defns))
	    *defs*))
      (let ((old-defs defns)
	    (new-defs 
	     (with-open-file (loadstr filename :direction :input)
			(setq *curr-lisp-code* nil)
			(setq *curr-checklist* nil)
			(nesl-loop loadstr defns t defns
				   :interactive nil :print print))))
	(if (not *error*) 
	  (progn
	    (with-open-file (outstr lsp-filename :direction :output
				    :if-exists :supersede 
				    :if-does-not-exist :create)
		(write-to-str `(in-package :nesl-lisp) outstr)
		(write-to-str `(check-types 
			     ',(extract-types-values *curr-checklist* old-defs)
			      *defs*) outstr)
		(dolist (code *curr-lisp-code*)
			(write-to-str code outstr)))
	    (let ((*error-output* nil))
		     (compile-file lsp-filename :output-file obj-filename))
	    (unless *dump-lisp-version* (delete-file lsp-filename))
	    new-defs)
	  new-defs)))))
    


(defun apropos-nesl (args definitions)
  (when (not (= (length args) 1))
    (nesl-error "Apropos should be in the form: \"apropos <name>\"."))
  (let* ((name (first args))
         (matches (nesl-apropos-all (string name) 
				  definitions)))
    (dolist (match matches)
       (format t "~%~(~a~) : ~a" 
	  (binding-name match)
	  (pprint-oneline-string (cnesl-full-type (binding-type match)))))
    definitions))

(defun nesl-apropos-all (name bindings)
  (if bindings
      (let* ((curr-bind (car bindings))
	     (func (binding-original-code curr-bind))
             (docstring (get-keyword :documentation 
				     (if func (func-other func) nil) nil))
             (fname (string (binding-name curr-bind))))
        (if (and (not (or (find #\- fname) (search "PRIM_" fname)))
                 (or (search name fname)
                     (and docstring 
                          (search name docstring :test #'char-equal))))
            (cons curr-bind (nesl-apropos-all name (cdr bindings)))
          (nesl-apropos-all name (cdr bindings))))
    nil))

(defun report-bug (stream)
  (declare (special *nesl-version* *nesl-date*))
  (multiple-value-bind 
      (second minute hour date month year) (get-decoded-time)
      (declare (ignore second))
      (format stream   "Current time: ~D/~D/~D ~2,'0D:~2,'0D~%" 
	      month date year hour minute))

  (format stream "NESL version: ~a (~a)~%" *nesl-version* *nesl-date*)
  ;;(format stream "NESL path: ~a~%" *nesl-path*) 
  ;;(format stream "*****NESL status:")
  ;;(show-status stream)
  (format stream "~%*****Machine information~%")
  (format stream "Machine type: ~s~%" (machine-type))
  (format stream "Machine version: ~s~%" (machine-version))
  (format stream "Software type: ~s~%" (software-type))
  (format stream "Software version: ~s~%" (software-version))
  (format stream "*****LISP information~%")
  (format stream "Lisp Implementation: ~s~%" (lisp-implementation-type))
  (format stream "Lisp Implementation version: ~s~%" 
	  (lisp-implementation-version))
  (format stream "Lisp features:")
  (if (eql stream t)
      (pprint *features*)
    (pprint *features* stream))
  )

(defun dump-nesl (dumpfile)
  (declare (special *username* *step-count*))
  (setq *username* nil)
  (setq *step-count* 0)
  (save-lisp-image dumpfile #'nesl)
  nil)

;;;;;;;;;;;;
;;; NESL'S TOPLEVEL LOOP
;;;;;;;;;;;;

(defun eval-dump (command definitions)
  (declare (ignore definitions))
  (cond ((eql (car command) 'world) 
	 (cond ((= (length command) 1)
		(dump-nesl "runnesl"))
	       ((and (= (length command) 2) (stringp (second command)))
		(dump-nesl (second command)))
	       (t 
		(nesl-error 
		 "Bad type for dump world, expecting a character string."))))
	((eql (car command) 'info) 
	 (cond ((= (length command) 1)
		(report-bug t))
	       ((and (= (length command) 2) (stringp (second command)))
		(with-open-file (ofile (merge-pathnames (second command))
				       :direction :output 
				       :if-exists :supersede)
		  (report-bug ofile)))
	       (t (nesl-error 
		   "Bad type for dump info, expecting a character string."))))
	(t
	 (nesl-error "Unknown top level command: DUMP ~a." (car command)))))

(defun eval-toplevel (form defs &optional out-str old-defs)
  (cond	((eql form 'help) (values defs (nesl-help)))
	((eql form 'exitall) (quit-lisp))
	((listp form)
	 (cond ((eql (car form) 'load-nesl) 
		(let ((val (typecheck-and-eval-exp (second (second form)) 
						   defs)))
		  (if (and (nesl-seq-p val) 
			   (eql (second (nesl-seq-type val)) 'char))
		      (load-nesl (coerce (nesl-seq-value val) 'string) defs)
		    (nesl-error "Load expects a string as an argument"))))
	       ((eql (car form) 'nload-nesl) 
		(let ((val (typecheck-and-eval-exp (second (second form)) 
						   defs)))
		  (if (and (nesl-seq-p val) 
			   (eql (second (nesl-seq-type val)) 'char))
		      (nload-nesl (coerce (nesl-seq-value val) 'string) defs)
		    (nesl-error "obj_load expects a string as an argument"))))
	       ((eql (car form) 'describe) (describe-nesl (cdr form) defs))
	       ((eql (car form) 'set) (eval-set (cdr form) defs))
	       ((eql (car form) 'show) (eval-show (cdr form) defs))
	       ((eql (car form) 'apropos) (apropos-nesl (cdr form) defs))
	       ((eql (car form) 'dump) (eval-dump (cdr form) defs))
	       (t (eval-nesl-toplevel form defs out-str old-defs))))
	(t (eval-nesl-toplevel form defs out-str old-defs))))

(defun add-nesl-variable (name val defs)
  (add-var-binding name (eval-exp val defs) defs 
		   (typecheck-top-exp val defs)))
	       

(defun init-nesl (defs)
  (declare (special *nesl-version* *nesl-date* *defs* *ready*))
  (format t "~%% NESL version ~a (~a) %" *nesl-version* *nesl-date*)
  (let* (
	 (defs (if (probe-file "~/.nesl")
		  (load-nesl "~/.nesl" *defs*)
		defs))
	 (defs (add-nesl-variable 'display (or (getenv "DISPLAY") "") defs))
	 (defs (add-nesl-variable 'user (or (getenv "USER") "") defs)))
    (format 
     t "~%~%% Use (nesl) when an error aborts you out of the interpreter. %~%~
            % Type help; for a list of the top level commands. %~%")
    (setq *original-defs* defs)
    (setq *defs* defs)))

(defun nesl-loop (in-stream defns &optional out-str old-defns &key interactive print)
  (setq *error* nil)
  (let ((*read-default-float-format* 'double-float)
	(*package* (find-package 'nesl-lisp)))
    (declare (special *prompt* *defs*))
    (setq *line-num* 0)
    (loop
     (when interactive
       (when (eql *step-count* 0) 
	 (setq defns (init-nesl defns)))
       (incf *step-count*)
       (format t "~%~a" *prompt*)
       (force-output))
     (catch 'nesl-error
       (catch 'nesl-runtime-error 
	 (when *eof* (setq *eof* nil) (return))
	 (let ((readval (cgolread in-stream interactive)))
	   (if (eql readval :exit) 
	       (return t)
	     (multiple-value-bind (new-defns value type var) 
	       (eval-toplevel readval defns out-str old-defns)
	       (setq defns new-defns)
	       (setq *defs* defns)
	       (when (and print type) 
;		     (format t "ANS = ~a~%" value)
		     (write-string (pprint-nesl-result value type var))
		     (force-output))))))))
    defns))

(defun nesl ()
  (declare (special *defs* *ready*))
  (setq *ready* t)
  (nesl-loop *standard-input* *defs* nil nil :interactive t :print t)
  t)

(defun user::nesl () 
  #+kcl (progn (read-line))
  (nesl))

(defun nesl-read ()
  (loop
   (catch 'nesl-error
     (when *eof* (setq *eof* nil) (return))
     (let ((readval (cgolread *standard-input* t)))
       (if (eql readval :exit) (return t)
	 (format t "~a~%~%" readval))))
   (force-output)))


