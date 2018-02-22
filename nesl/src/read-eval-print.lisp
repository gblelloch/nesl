;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp) 

;;;;;;;;;;;;;;;;;;
;;; THIS KEEPS ALL THE NESL STATE (functions, types, variables...)
;;;;;;;;;;;;;;;;;;

(defparameter *definitions* (make-defs-table))
(defparameter *stderr* *error-output*)

;;;;;;;;;;;;;;;;;;
;;; THESE ARE VARIOUS TOP LEVEL PARAMETERS
;;;;;;;;;;;;;;;;;;

(defparameter *eof* nil)
(defparameter *debug* nil)
(defparameter *redefine-default* 'on)
(defparameter *cnesl-syntax* t)
(defparameter *verbose* t)
(defparameter *argument_check* nil)
(defparameter *step-count* 0)
(defparameter *background-list* nil)
(defparameter *trace-list* nil)
(defparameter *profile-list* nil)
(defparameter *current-fundef* nil)
(defparameter *current-typedef* nil)
(defparameter *max-print-length* 100)
(defparameter *default-list* nil)
(defparameter *current-config* nil)
(defparameter *config-list* nil)
(defparameter *editor* nil)
(defparameter *last-file* "")
(defparameter *in-nesl* nil)
(defparameter *fully-loaded* nil)
(defparameter *nesl-server* nil)

;;;;;;;;;;;;;;;;;;
;;; KEEP TRACK OF WHAT ARE SPECIAL FORMS AND TOP LEVEL COMMANDS
;;;;;;;;;;;;;;;;;;

(defparameter *special-forms* 
  '(|,| function datatype =))

(defparameter *show-commands*
  '((bugs . 0) (config . 0) (configs . 0) 
    (code . 1) (compiled . 2) (status . 0)))

(defparameter *set-commands*
  '((trace . -1) (profile . -1) 
    (print_length . 1) (arg_check . 1)
    (verbose . 1) (debug . 1) (redefine . 1)
    (lnesl . 1) (config . 1)
    (memory_size . 1) (rsh_command . 1) 
    (editor . 1)))

;;;;;;;;;;;;;;;;;;
;;; KNOWN NESL BUGS
;;;;;;;;;;;;;;;;;;

(defparameter *nesl-bugs* `(
"Certain errors will drop you out of the read/eval/print loop into
the lisp error handler.   You can restart by typing (nesl)."

"The function rand does not work properly when called in parallel using
a constant argument.  To get around this bug, instead of using an expression
of the form
  {rand(100)+i: i in a}
use an expression of the form
  {rand(j) + i: i in a; j in dist(100,#a)} ."

"max_val(), min_val(), max_scan(), and min_scan() do not work
on character sequences."

"Timings cannot be nested."

"IEEE Not-a-Number and Infinity floats cannot be returned to top-level."

"Using A <- B on the CM-5 when A is a sequence of tuples and there are
collisions (repeated indices in B) can give erouneous results."

"Please read the appendix of the manual for some notes on the running
time of certain constructs."

))

;;;;;;;;;;;;;;;;;;
;; Makes sure that users don't use t and f as variable names
;;;;;;;;;;;;;;;;;;

(add-nondefinable-constant 't *definitions*)
(add-nondefinable-constant 'f *definitions*)
(add-nondefinable-constant 'time *definitions*)

;;;;;;;;;;;;;;
;;; ERROR HANDLER
;;;;;;;;;;;;;;

(defun nesl-error (format-string &rest args)
  (let ((eo *stderr*))
    (if *current-fundef*
	(if *cnesl-syntax*
	    (format eo "~%Error in Function definition ~a.  " *current-fundef*)
	  (format eo "~%Error in (DEFOP (~a ..) ..).  " *current-fundef*))
      (if *current-typedef*
	  (if *cnesl-syntax*
	      (format eo "~%Error in Datatype declaration ~a.  " 
		      *current-typedef*)
	    (format eo "~%Error in (DEFREC (~a ..) ..).  " *current-typedef*))
	(format eo "~%Error at top level.  ")))
    (cond (*debug*
	   (format eo "~%")
	   (apply 'error format-string args))
	  (t
	   (format eo "~%")
	   (apply 'format eo format-string args)
	   (format eo "~%")
	   (throw 'nesl-error :error)))))

;;;;;;;;;;;;;
;;; HELP FACILITIES
;;;;;;;;;;;;;

(defun describe-nesl (form definitions)
  (declare (special *special-forms* *set-commands* *show-commands* *doclist*))
  (when (not (= 1 (length form)))
    (nesl-error 
     "Bad syntax for describe, should be in form: describe <fname>;"))
  (let* ((funname (car form))
	 (binding (get-binding-definition funname definitions)))
    (cond ((not funname)
	   (dolist (fun *doclist*)
	      (if (and (symbolp fun) (get-binding-definition fun definitions))
		  (let* ((binding (get-binding-definition fun definitions))
			 (fundef (binding-original-code binding))
			 (interface (pprint-nesl-string 
				     (cons (func-name fundef)
					   (func-arguments fundef))))
			 (doc (get-keyword :shortdoc (func-other fundef) nil)))
		    (if doc
			(format t "~25a ~a" interface doc)
		      (format t "~a" interface))))))
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
		  (funcp (func-arguments fundef))
		  (code (func-body fundef))
		  (serial? (pscode-serial-only?
			    (binding-compiled-code binding)))
		  (doc (get-keyword :documentation (func-other fundef) nil)))
	     (when funcp
	       (format t "~%INTERFACE:~% ~a~%"  
		       (pprint-nesl-string interface)))
	     (format t (if serial? "~%TYPE: (callable in serial only)~%~% ~a~%"
			 "~%TYPE:~%~% ~a~%")
		     (pprint-oneline-string (cnesl-full-type type)))
	     (when doc  (format t "~%DOCUMENTATION:~%~% ~a~%" doc))
	     (when funcp
	       (if (special-body? code)	     
		   (format t "~%CODE: *Primitive*~%")
	        (let ((lines (cnesl-exp code)))
		  (format t "~%CODE:~%~a~%" (pprint-nesl-string-rec lines 2 6))
		  (when (> (length lines) 6)
		  (format 
		   t "~%Use \"show code ~(~a~);\" to see all the code."
		   funname)))))))
	  (t (format t "~a not found." funname)))
    funname))

(defun show-status (stream)
  (format stream "~%verbose ~a~%arg_check ~a~%print_length = ~a"
	  (if *verbose* "on" "off")
	  (if *argument_check* "on" "off")
	  *max-print-length*)
  (when *editor*
    (format stream "~%editor = ~s" *editor*))
  (when *trace-list*
    (format stream "~%traced functions:~% ~{ ~(~a~)~}" *trace-list*))
  (when *profile-list*
    (format stream "~%profiled functions:~% ~{ ~(~a~)~}" *profile-list*))
  (when *background-list*
    (format stream "~%background variables:")
    (dolist (item (reverse *background-list*) )
	    (if (check-background-result (third item))
		(format stream "~%  ~a (done)" (first item))
	      (format stream "~%  ~a (waiting on ~a)" 
		      (first item) 
		      (config-machine-name (fifth (third item)))))))
  (format stream "~%"))

(defun report-bug (stream)
  (declare (special *nesl-version* *nesl-date* *nesl-path*))
  (multiple-value-bind 
      (second minute hour date month year) (get-decoded-time)
      (declare (ignore second))
      (format stream   "Current time: ~D/~D/~D ~2,'0D:~2,'0D~%" 
	      month date year hour minute))

  (format stream "NESL version: ~a (~a)~%" *nesl-version* *nesl-date*)
  (format stream "NESL path: ~a~%" *nesl-path*) 
  (format stream "*****NESL status:")
  (show-status stream)
  (format stream "*****NESL configuration:")
  (print-config *current-config* stream)
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

(defun show-code (funname definitions)
  (let* ((binding (get-binding-definition funname definitions)))
    (if binding
	(if (is-function-type? (func-type (binding-original-code binding)))
	    (if (special-body? (func-body (binding-original-code binding)))
		(format t "~%~a is a primitive." funname)
	      (pprint-nesl (binding-original-code binding)))
	  (format t "~%~a is not a function definition." funname))
      (format t "~%Function ~a not found." funname))))

(defun show-compiled-code (funname pversion definitions)
  (let ((binding (get-binding-definition funname definitions)))
    (if binding
	(let* ((pscode (binding-compiled-code binding))
	       (code (if pversion (pscode-parallel pscode) 
		       (pscode-serial pscode))))
	  (pprint-nesl (code-compiled code)))
      (format t "Function ~a not found." funname))))

(defparameter *help-string*
"
NESL top-level forms:
  function <name> <pattern> [: <typedef>] = <exp>;  -- Function Definition
  datatype <name> <typedef>;                        -- Record Definition
  <pattern> = <exp>;                                -- Top level Assignment
  <exp>;                                            -- Any NESL expression

Top-level Commands:
  help; OR ?                    -- Print this message.
  load [<exp>];                 -- Load a file.  If no arg, reload last file.
  describe <funcname>;          -- Describe a NESL function.
  apropos <name>;               -- List functions which contain <name>.
  set arg_check {on,off};       -- Set the argument check switch.
  set trace <funcname> <n>;     -- Trace a NESL function.
                                     0=off, 1=fname, 2=args, 3=vars, 4=vals
  set profile <funcname> {on,off}; -- Set profiling for function <funcname>.
  set print_length <n>;         -- Set maximum sequence print length.
  set verbose {on,off};         -- Set the verbose switch.
  set editor <pathname>;        -- Set the editor.
  show status;                  -- List settings of current environment.
  show bugs;                    -- List the known bugs.
  show code <funcname>;         -- Show the code for a function.
  dump vcode <filename> <exp>;  -- Dump VCODE for <exp> to file <filename>.
  dump world [<filename>];      -- Dump current NESL environment to a file.
  dump info [<filename>];       -- Dump info for bug reports (default=stdout).
  edit [<filename>];            -- Edit & load a NESL file (default=last file).
  <pattern> |= <exp>;           -- Assign to a file variable.
  exit;                         -- Exit NESL.
  lisp; or ^D                   -- Go to the Common Lisp interpreter.

Commands for running VCODE on remote machines:
  defconfig <name> <args>; -- Define a new configuration.
  set config <config>;     -- Set the current configuration to <config>.
  set memory_size <n>;     -- Set the memory size of the current configuration.
  show config;             -- List the properties of the current configuration.
  show configs;            -- List the available configurations.
  <name> &= <exp> [,mem := <n>] [,max_time := <n>] [,config := <config>];
                           -- Execute exp in the background.
  get <name>;              -- Get a background result.
")

(defparameter *short-help-string*
"
NESL top-level forms:
  function <name> <pattern> [: <typespec>] = <exp>;  -- Function Definition
  datatype <name> <typeexp> [:: <typebind>];         -- Record Definition
  <pattern> = <exp>;                                 -- Top level Assignment
  <exp>;                                             -- Any NESL expression

Top-level Commands:
  help;                    -- Print this message.
  describe <funcname>;     -- Describe a NESL function.
  apropos <name>;          -- List functions which contain <name>.
  set arg_check {on,off};  -- Set the argument check switch.
  show code <funcname>;    -- Show the code for a function.
  load <exp>;              -- Load a file.
  show bugs;               -- List the known bugs.
  exit; or ^D;             -- exit nesl.
")

(defun cnesl-help ()
  (format t *help-string*))

(defun nesl-list-bugs ()
  (let ((i 0))
    (dolist (bug *nesl-bugs*)
      (format t "~%~a: ~a~%" (incf i) bug))))

;;;;;;;;;;;;
;;; STUFF FOR LOADING FILES
;;;;;;;;;;;;

(defun file-check (filename suffix type)
  (cond ((and (> (length filename) (length suffix))
	      (string-equal suffix
			    (subseq filename (- (length filename) 
						(length suffix))
				    (length filename)))
	      (probe-file filename))
	 (cons type filename))
	((probe-file (concatenate 'string filename suffix))
	 (cons type (probe-file (concatenate 'string filename suffix))))))

(defun get-filename-and-type (filename)
  (declare (special *obj-tailer*))
  (let ((type-name
	 (cond ((file-check filename ".nesl" :nesl))
	       ((file-check filename ".lnesl" :lnesl))
	       ((and (not *nesl-server*) 
		     (file-check filename ".lisp" :lisp)))
	       ((and (not *nesl-server*) 
		     (file-check filename *obj-tailer* :lisp)))
	       ((probe-file filename) (cons :nesl filename))
	       (t (cons nil filename)))))
    (values (car type-name) (cdr type-name))))

(defun wrap-comment (str)
  (format nil (if *in-nesl* "~%% ~a %" "~%; ~a") str))

(defun load-nesl (filename &key (verbose t) (print nil))
  (when (or (not (stringp filename)) (zerop (length filename)))
    (nesl-error 
     "The second argument to LOAD must be a nonempty character string."))
  (multiple-value-bind (type name) 
    (get-filename-and-type (namestring (merge-pathnames filename)))
    (cond ((not type)
	   (nesl-error "File ~a does not exist." name))
	  ((eql type :lisp)
	   (load name :verbose verbose :print print))
	  ((eql type :lnesl)
	   (with-open-file (loadstr name :direction :input)
	     (when verbose 
	       (format t (wrap-comment "Loading ~a.") name))
	     (let ((*cnesl-syntax* nil)
		   (*verbose* print))
	       (declare (special *cnesl-syntax* *verbose*))
	       (nesl-loop loadstr :interactive nil :print print))))
	  ((eql type :nesl)
	   (when (not (fboundp 'cgolread))
	     (nesl-error "CNESL is not loaded, use (load-cnesl-syntax)."))
	   (with-open-file (loadstr (probe-file name) :direction :input)
	     (when (and verbose (not (eql verbose 'nil)))
	       (format t (wrap-comment "Loading ~a.") name))
	     (let ((*cnesl-syntax* t)
		   (*verbose* print))
	       (declare (special *cnesl-syntax* *verbose*))
	       (nesl-loop loadstr :interactive nil :print print)))))))

(defun trans-nesl-to-cnesl (filename)
  (let ((infile (concatenate 'string filename ".lnesl"))
	(outfile (concatenate 'string filename ".nesl"))
	(*cnesl-syntax* nil))
    (when (probe-file outfile) (delete-file outfile))
    (with-open-file (instr infile :direction :input)
      (with-open-file (outstr outfile :direction :output)
	(loop
	 (let ((readval (nesl-read instr nil)))
	   (if (eql readval :exit)
	       (return)
	     (progn
	       (pprint-nesl readval outstr)
	       (format outstr " $~%")))))))))

;;;;;;;;;;;;;;;;
;;; EVALUATING A NESL FORM
;;;;;;;;;;;;;;;;

;; This adds a line to the beginning and end of the code that prints
;; when interpretation is starting and ending.
(defun verbose-wrapper (form wrap_p)
  (if wrap_p
      `(let (start (print_string "Running..
"))
	 (let (result ,form)
	   (let (end (print_string "Exiting.."))
	     result)))
    form))

(defun make-main (form verbose definitions)
  (multiple-value-bind (val type vars)
    (add-function-binding 
     (make-func :name 'main :body (verbose-wrapper form verbose)) definitions)
    (declare (ignore val vars))
    (when *verbose* (format t "~%Compiling..") (force-output))
    (cons (third (car type)) (cdr type))))

(defun dump-exp-to-file (filename exp definitions)
  (when (not (stringp filename))
    (nesl-error "The second argument to dump_code must be a string."))
  (make-main exp nil definitions)
  (write-vcode-file 'main filename definitions))

(defun dont-need-to-eval? (form)
  (or (nesl-constant-p form)
      ;;(pscode-p form) -- need to make sure it does'nt count constants.
      (and (listp form)
	   (or (and (pscode-p (car form))
		    (pscode-datatype? (car form)))
	       (and (eql (car form) 'pair) 
		    (dont-need-to-eval? (second form))
		    (dont-need-to-eval? (third form)))))))

(defun eval-nesl (form definitions)
  (multiple-value-bind (new-form type)
    (typecheck-const form nil definitions)
    (cond ((dont-need-to-eval? new-form)
	   (values new-form type))
	  ((listp form)
	   (let ((rtype (make-main form *verbose* definitions)))
	     (values 
	      (group-data 
	       (run-vcode 'main (flatten-type (car rtype) definitions) 
			  *current-config* definitions)
	       (car rtype) definitions)
	      rtype)))
	  ((symbolp form)
	   (let ((binding (get-binding-definition form definitions)))
	     (if (not binding)
		 (nesl-error "Variable ~a is undefined." form)
	       (let* ((fundef (binding-original-code binding))
		      (code (func-body fundef))
		      (type (func-type fundef)))
		 (values (if (is-function-type? type)
			     (binding-compiled-code binding)
			   (if (and (listp code) (eql (car code) 'let))
			       (eval-nesl code definitions)
			     code))
			 type)))))
	  (t (nesl-error "Invalid form: ~a" form)))))

(defun eval-background (form definitions)
  (declare (special *current-config* *background-list*))
  (let* ((var (second form))
	 (args (third form))
	 (exp (first-pair args))
	 (mem (second-pair args))
	 (time (third-pair args))
	 (time (if (eql time 'default) 
		   (if (eql (config-max-time *current-config*) -1)
		       60
		     (config-max-time *current-config*))
		 time))
	 (machine (fourth-pair args))
	 (config (if (eql machine 'default) *current-config*
		   (get-configuration machine)))
	 (mem (if (eql mem 'default) (config-memory-size config) mem)))
    (when (not (integerp mem))
      (nesl-error "The MEM argument must be an integer."))
    (check-valid-varname var definitions)
    (let* ((rtype (make-main exp *verbose* definitions))
	   (out-err-files 
	    (run-vcode-background 'main mem time config definitions)))
      (push (list var rtype out-err-files) *background-list*)
      (when *verbose* 
	(format t "background ~(~a~) : ~a" var 
		(pprint-oneline-string (cnesl-full-type rtype)))))))

(defun remassoc (name list)
  (if (null list) nil
    (if (eql name (caar list))
	(cdr list)
      (cons (car list) (remassoc name (cdr list))))))

(defun get-background (name definitions)
  (let ((type-files-config (cdr (assoc name *background-list*))))
    (when (not type-files-config)
      (nesl-error "No background job for ~a." name))
    (cond ((check-background-result (second type-files-config))
	   (setq *background-list* (remassoc name *background-list*))
	   (let* ((out-err-files (second type-files-config))
		  (type (first type-files-config))
		  (config (fifth out-err-files))
		  (flattype (flatten-type (car type) definitions))
		  (res (get-background-result out-err-files flattype config))
		  (val (group-data res (car type) definitions)))
	     (add-global-variable name val type definitions)
	     (values val type name)))
	  (t (format t "~%Variable waiting for result.")))))

(defun eval-assign (form definitions &optional (filevar-name nil))
  (let ((pattern (first form))
	(body (second form))
	(other (third form)))
    (multiple-value-bind (val type)
      (eval-nesl body definitions)
      (let ((binds (match-types pattern val 
				(instantiate-fun-type type) definitions)))
	(when (null binds)
	  (nesl-error "Pattern ~a does not match value~a~%in an assignment."
		      (pprint-nesl-short-string pattern)
		      (pprint-nesl-string val 2)))
	(dolist (bind binds)
          (add-global-variable 
	   (first bind)
	   (if filevar-name
	       `(let (,pattern (,(get-pscode 'prim_read_file_variable)
				(pair 
				 ,val 
				 (pair ,(string (first bind)) ,filevar-name))))
		  ,(first bind))
	     (second bind))
	   (clean-type (third bind) definitions)
	   definitions
	   other))
	(if filevar-name
	    (format t "~%~(~a~) : ~a" 
		    (pprint-oneline-string (cnesl-exp pattern))
		    (pprint-oneline-string (cnesl-full-type type)))
	(values val type pattern))))))

(defun eval-load (form definitions)
  (if (second form)
      (multiple-value-bind (val type) 
	(eval-nesl (first-pair (second form)) definitions)
	(when (not (equal type '((vector char))))
	  (nesl-error 
	   "Bad type for load, expecting a character string."))
	(let ((filename (coerce (vcode-vector-data (third (second val))) 
				'string))
	      (verbosep (not (eql (second-pair (second form)) 'f)))
	      (printp (not (eql (third-pair (second form)) 'f)))
	      (save_filename (not (eql (fourth-pair (second form)) 'f))))
	  (when save_filename (setq *last-file* filename))
	  (load-nesl filename :verbose verbosep :print printp)))
    (load-nesl *last-file*)))

(defun eval-edit (form definitions)
  (declare (ignore definitions))
  (when (not (stringp (or (second form) *last-file*)))
    (nesl-error "Bad argument for edit.  Use format: edit \"filename\";"))
  (when (second form)  (setq *last-file* (second form)))
  (when (not *editor*)
    (nesl-error "No editor specified, use: set editor <ename>;"))
  (multiple-value-bind (filetype filename)
    (get-filename-and-type *last-file*)
    (declare (ignore filetype))
    (run-shell-line (format nil "~a ~a" *editor* filename))
    (format t "~%Load?: [yes] ")
    (let ((answer (read-line)))
      (when (or (equal answer "yes") (equal answer ""))
	(load-nesl filename)))))

(defun eval-shell (form)
  (cond ((and (= (length form) 1) (stringp (first form)))
	 (remote-shell (first form) *current-config*))
	((and (= (length form) 2) (stringp (second form)))
	 (remote-shell (second form) (get-configuration (first form))))
	(t (nesl-error "Bad argument for shell.  ~
                        Use format: shell [<config>] \"<command>\";"))))

;;;;;;;;;;;;;;;;;;;
;;; EVALUATING A TOP LEVEL FORM....yes, it is a giant case statement
;;;;;;;;;;;;;;;;;;;

(defun onoff? (token command)
  (cond ((eql token 'on) t)
	((eql token 'off) nil)
	(t (nesl-error 
	    "Expected either ON or OFF for command ~a." 
	    (car command)))))

(defun car-check (form name) 
  (and (not *nesl-server*) (eql (car form) name)))

(defun eval-set (command definitions)
  (let ((com (cdr (assoc (car command) *set-commands*))))
    (when (not com)
      (nesl-error "Unknown top level command: SET ~a" (car command)))
    (when (and (not (minusp com)) (not (= (length (cdr command)) com)))
      (nesl-error "Wrong number of arguments to \"set ~(~a~)\"" (car command)))
    (cond ((eql (car command) 'trace)
	   (cond ((= (length command) 3)
		  (nesl-trace (second command) (third command) definitions))
		 ((and (= (length command) 2) (eql (second command) 'off))
		  (trace-off definitions))
		 (t (nesl-error "Bad format for \"set trace\"."))))
	  ((eql (car command) 'profile)
	   (cond ((= (length command) 3)
		  (nesl-time-switch (second command) 
				    (onoff? (third command) command) 
				    definitions))
		 ((and (= (length command) 2) (eql (second command) 'off))
		  (time-off definitions))
		 (t (nesl-error "Bad format for \"set profile\"."))))
	  ((eql (car command) 'print_length)
	   (when (not (integerp (second command)))
	     (nesl-error 
	      "The argument to SET PRINT_LENGTH must be an integer."))
	   (setq *max-print-length* (second command)))
	  ((eql (car command) 'arg_check)
	   (setq *argument_check* (onoff? (second command) command))
	   (checkall definitions))
	  ((eql (car command) 'verbose)
	   (setq *verbose* (onoff? (second command) command)))
	  ((eql (car command) 'redefine)
	   (setq *redefine-default* (second command)))
	  ((car-check command 'debug)
	   (setq *debug* (onoff? (second command) command)))
	  ((and (not *nesl-server*) (eql (car command) 'editor))
	   (when (not (stringp (second command)))
	     (nesl-error "Editor must be a string"))
	   (setq *editor* (second command)))
	  ((eql (car command) 'lnesl)
	   (setq *cnesl-syntax* (not (onoff? (second command) command))))
	  ((and (not *nesl-server*) (eql (car command) 'config) )
	   (set-current-config (get-configuration (second command)) 
			       definitions)
	   (print-config *current-config* t))
	  ((and (not *nesl-server*) (eql (car command) 'memory_size))
	   (set-memsize (second command) *current-config*))
	  ((eql (car command) 'rsh_command)
	   (set-rsh-command (second command) *current-config*)))))

(defun eval-show (command definitions)
  (let ((com (cdr (assoc (car command) *show-commands*))))
    (when (not com)
      (nesl-error "Unknown top level command: SHOW ~a" (car command)))
    (when (not (= (length (cdr command)) com))
      (nesl-error "Wrong number of arguments to \"show ~(~a~)\""(car command)))
    (cond ((eql (car command) 'bugs) (nesl-list-bugs))
	  ((eql (car command) 'config) (print-config *current-config* t))
	  ((eql (car command) 'configs) (list-configs *config-list*))
	  ((eql (car command) 'status) (show-status t))
	  ((eql (car command) 'code)
	   (show-code (second command) definitions))
	  ((eql (car command) 'compiled)
	   (show-compiled-code (second command) (eql 't (third command))
			       definitions)))))

(defun eval-dump (command definitions)
  (cond ((eql (car command) 'world) 
	 (cond ((= (length command) 1)
		(dump-nesl (concatenate 'string *nesl-path* "bin/runnesl")))
	       ((and (= (length command) 2) (stringp (second command)))
		(dump-nesl (second command)))
	       (t 
		(nesl-error 
		 "Bad type for dump world, expecting a character string."))))
	((eql (car command) 'vcode) 
	 (dump-exp-to-file (second command) (third command) definitions))
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

(defun eval-and-set-nesl (form defs)
  (multiple-value-bind (value type) 
    (eval-nesl form defs)
    (add-global-variable 'it value type defs)
    (values value type 'it)))

(defun eval-toplevel (form defs)
  (cond ((func-p form)
	 (let ((*current-fundef* (func-name form)))
	   (add-function-binding form defs)))
	((eql form 'help) (cnesl-help))
	((eql form 'exitall) (quit-lisp))
	((listp form)
	 (cond ((eql (car form) 'set) (eval-set (cdr form) defs))
	       ((eql (car form) 'show) (eval-show (cdr form) defs))
	       ((car-check form 'dump) (eval-dump (cdr form) defs))
	       ((car-check form 'progn) (eval form))
	       ((car-check form 'shell) (eval-shell (cdr form)))
	       ((eql (car form) 'pprint) (pprint-nesl  (second form)))
	       ((eql (car form) 'assign) (eval-assign (cdr form) defs))
	       ((car-check form 'file-assign) 
		(eval-assign (second form) defs (third form)))
	       ((eql (car form) 'defrec) (parse-defrec (cdr form) defs))
	       ((eql (car form) 'defprimtype)
		(add-prim-type (second form) defs))
	       ((car-check form 'edit) (eval-edit form defs))
	       ((eql (car form) 'deftypeclass)
		(add-type-class (second form) (third form) defs))
	       ((eql (car form) 'config) (push (second form) *config-list*))
	       ((eql (car form) 'load-nesl) (eval-load form defs))
	       ((eql (car form) 'describe) (describe-nesl (cdr form) defs))
	       ((eql (car form) 'apropos) (apropos-nesl (cdr form) defs))
	       ((eql (car form) 'background) (eval-background form defs))
	       ((eql (car form) 'get-background)
		(get-background (second form) defs))
	       ((eql (car form) 'ptransp)
		(pprint-nesl (conv-exp (strip-exp (second form) defs) t nil)))
	       (t (eval-and-set-nesl form defs))))
	(t (eval-and-set-nesl form defs))))

;;;;;;;;;;;;;
;;; INIT NESL
;;;;;;;;;;;;;

(defun set-nesl-variable (name val defs &key (redefinable? 'off) (doc nil))
  (multiple-value-bind (val type) 
    (eval-nesl val defs)
    (let ((*redefine-default* redefinable?))
      (declare (special *redefine-default*))
      (add-global-variable name val type defs (list :documentation doc)))))

;;(defun set-current-config (config defs)
;;  (setq *current-config* config)
;;  (set-nesl-variable 'x_plot_file (config-plot-file config) defs))

(defun set-current-config (config defs)
  (setq *current-config* config)
  (format t "~%~%% Setting machine configuration ~a.... %" 
	  (config-name config))
  (let* ((*verbose* nil)
	 (ivect (make-empty-vector 'int defs))
	 (check (catch 'nesl-error
		  (eval-assign `((pair min_int max_int)
				 (pair (max_val ,ivect) (min_val ,ivect)))
			       defs))))
    (declare (special *verbose*))
    (when (eql :error check)
      (format T "
**** ERROR: The configuration ~a is not set up properly.
**** Is there a compiled version of the VCODE interpreter:
****   ~a?
**** Is the interpreter compiled for another machine?~%"
	      (config-name config)
	      (config-interp-file config))))
  (set-nesl-variable 'xps_plot_file (config-plot-file config) defs))

(defun init-machine-configuration (defs)
  (declare (special *current-config* *config-list*))
  (let* ((config (or (getenv "NESL_CONFIG") "local"))
	 (config-name (intern (string-upcase config) 'nesl-lisp)))
    (set-current-config (get-configuration config-name) defs)))

(defun init-nesl (defs)
  (declare (special *nesl-path* *nesl-version* *nesl-date*))
  (let ((*package* (find-package 'nesl-lisp)))
    (declare (special *package*))
    (format t "~%% NESL version ~a (~a) %" *nesl-version* *nesl-date*)
    (init-machine-configuration defs)
    (when (and (not *nesl-server*) (probe-file "~/.nesl"))
      (load-nesl "~/.nesl"))
    (set-nesl-variable 'nesl_path *nesl-path* defs)
    (set-nesl-variable 'display (or (getenv "DISPLAY") "") defs
		       :redefinable? 'on
		       :doc
		       "The display variable inherited from your environment.")
    (set-nesl-variable 'user (or (getenv "USER") "") defs
		       :redefinable? 'on
		       :doc
		       "The username inherited from your environment.")
    (format 
     t "~%~%% Use (nesl) when an error aborts you out of the interpreter. %~%~
            % Type help; for a list of the top level commands. %~%")))

(defun nesl-announce ()
  (setq *fully-loaded* t)
)

;;;;;;;;;;;;;
;;; THE NESL READ/EVAL/PRINT LOOP
;;;;;;;;;;;;;

(defun nesl-read (in-stream interactive)
  (if *cnesl-syntax*
      ;; this is a real hack since cgolread doesn't work
      (or (cgolread in-stream interactive) :exit)
    (let ((result (nesl-read-toplevel (read in-stream nil :exit))))
      result)))

(defun nesl-loop (in-stream &key interactive print)
  (let ((*in-nesl* t)
	(*package* (find-package 'nesl-lisp))
	(*read-default-float-format* 'double-float)
	(*line-num* 1)
	(definitions *definitions*))
    (declare (special *in-nesl* *package* *read-default-float-format* 
		      *line-num*))
    (loop
     (when interactive 
       (when (eql *step-count* 0) (init-nesl *definitions*))
       (incf *step-count*)
       (format t "~%<Nesl> " ) (force-output))
     (if *debug*
	 (catch 'nesl-error
	   (when *eof* (setq *eof* nil) (return))
	   (let ((readval (nesl-read in-stream interactive)))
	     (if (eql readval :exit)
		 (return)
	       (multiple-value-bind (value type var) 
		 (eval-toplevel readval definitions)
		 (when (and print type) 
		   (write-string (pprint-nesl-result value type var))
		   (force-output))))))
       (nesl-ignore-errors
	(catch 'nesl-error
	  (when *eof* (setq *eof* nil) (return))
	  (let ((readval (nesl-read in-stream interactive)))
	    (if (eql readval :exit)
		(return)
	      (multiple-value-bind (value type var) 
		(eval-toplevel readval definitions)
		(when (and print type) 
		  (write-string (pprint-nesl-result value type var))
		  (when *nesl-server*  (format t "~%"))
		  (force-output)))))))))
    (when interactive (format t "~%") (force-output))
    (values)))

(defun interactive-nesl ()
  (nesl-loop *standard-input* :interactive t :print t))

(defun time-mark ()
  (multiple-value-bind 
   (second minute hour date month year) (get-decoded-time)
   (format nil "server_~a~a~a" hour minute second)))

(defun run-nesl-server ()
  (declare (special *username*))
  (setq *username* (time-mark))
  (setq *stderr* (open "/dev/stderr" :direction :output))
  (ignore-errors 
    (nesl-loop *standard-input* :interactive nil :print t))
  (format t "~%")
  (quit-lisp))

(defun #-cmu user::dump-nesl-server 
       #+cmu common-lisp-user::dump-nesl-server 
       (filename)
  (setq *nesl-server* t)
  (let ((*redefine-default* 'off))
    (declare (special *redefine-default*))
    (load-nesl (concatenate 'string *nesl-path* *srcdir* "short-io.lnesl")))
  (init-nesl *definitions*)
  (setq *verbose* nil)
  (setq *help-string* *short-help-string*)
  (set-current-config (get-configuration 'server) *definitions*)
  ;;(save-lisp-image "~scandal/bin/server-nesl" nil)
  (save-lisp-image filename #'run-nesl-server))

(defun nesl () 
  (if (not *fully-loaded*)
      (format t
       "Error: Nesl is not fully loaded.  You need to reload the files.")
    (interactive-nesl)))

(defun #-cmu user::nesl
       #+cmu common-lisp-user::nesl
       () (nesl))

;;; This checks if the vcode subprocess is working
;;(defun test-eval ()
;;  (let ((*verbose* nil))
;;    (declare (special *verbose*))
;;    (when (eql :error (catch 'nesl-error
;;			(eval-nesl (list (get-pscode 'not) t) *definitions*)))
;;      (format T "
;;**** ERROR: Your configuration is not set up properly.
;;**** Is there a compiled version of the VCODE interpreter (nesl/bin/vinterp)?
;;**** Is the interpreter compiled for another machine?~%")
;;      (throw 'load-error :error))))

(defun dump-nesl (dumpfile)
  (declare (special *username* *step-count*))
  (setq *username* nil)
  (setq *step-count* 0)
  (save-lisp-image dumpfile #'nesl)
  nil)
