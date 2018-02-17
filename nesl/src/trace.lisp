;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

(defun make-spaces (n) 
   (make-sequence 'string n :initial-element #\space))

(defun debug-var (var indent_n)
  (let ((++code (get-pscode '++)))
    `(,++code
      (pair ,(format nil "~a~a = " (make-spaces indent_n) var)
		  (,++code 
		   (pair (,(get-pscode 'abrev_string) ,var)
			       ,(format nil "~%")))))))

(defun debug-binding-r (vars indent_n)
  (if (= 1 (length vars))
      (debug-var (car vars) indent_n)
    (list (get-pscode '++) 
	  (list 'pair (debug-var (car vars) indent_n) 
		(debug-binding-r (cdr vars) indent_n)))))

(defun debug-binding (pattern print-vals? indent_n)
  (list (get-pscode 'print_string)
	(if print-vals?
	    (debug-binding-r (bindvars-exp pattern) indent_n)
	  (format nil "  Let: ~a~%" (pprint-nesl-short-string pattern)))))

(defun trace-let (exp print-vals?)
  (if (member (first (second exp)) '(timer-start timer-result timer-end))
      `(let ,(second exp) ,(trace-exp (third exp) print-vals?))
    `(let ,(second exp)
       (let (x-trace ,(debug-binding (first (second exp)) print-vals? 4))
	 ,(trace-exp (third exp) print-vals?)))))

(defun trace-exp (exp print-vals?)
  (cond	((nesl-constant-p exp) exp)
	((pscode-p exp) exp)
	((symbolp exp) exp)
	((listp exp)
	 (cond ((eql (car exp) 'let) 
		(trace-let exp print-vals?))
	       ((eql (car exp) 'over) 
		exp)
	       ((eql (car exp) 'if)
		`(if ,(trace-exp (second exp) print-vals?)
		     ,(trace-exp (third exp) print-vals?)
		   ,(trace-exp (fourth exp) print-vals?)))
	       ((eql (car exp) 'pair)
		`(pair ,(trace-exp (second exp) print-vals?)
			     ,(trace-exp (third exp) print-vals?)))
	       (t 
		`(,(first exp) ,(trace-exp (second exp) print-vals?)))
	       ))
	(t (nesl-error "Internal error: Invalid expression, ~s." exp))))

(defun trace-function (function level defs)
  (declare (ignore defs))
  (if (zerop level)
      function
    (let ((name (func-name function))
	  (args (car (func-arguments function)))
	  (body (func-body function))
	  (type (func-type function))
	  (other (func-other function))
	  (pstring (get-pscode 'print_string)))
      (make-func
       :name name :arguments (list args) :type type :other other
       :body 
       `(let (x-trace (,pstring ,(format nil "Entering ~a~%" name)))
	  (let (result 
		,(cond ((= level 1) body)
		       ((= level 2) 
			`(let (y-trace ,(debug-binding args t 2)) ,body))
		       ((= level 3) (trace-exp body nil))
		       ((>= level 4)
			`(let (y-trace ,(debug-binding args t 2))
			   ,(trace-exp body t)))))
	    (let (z-trace (,pstring ,(format nil "Leaving ~a~%" name)))
	      ,(if (or (= level 2) (>= level 4))
		   `(let (x-trace ,(debug-binding 'result t 2)) result)
		 'result))))))))

(defparameter *first-time-trace* t)

(defun nesl-trace (function-name level env)
  (declare (special *trace-list*))
  (when (or (not (symbolp function-name)) (not (numberp level)))
    (nesl-error "Set trace should be in the format:~%  ~
                     set trace <funname> <n>"))
  (let ((binding (get-binding-definition function-name env)))
    (if (not binding) (format t "~%~a is not defined." function-name)
      (if (special-body? (func-body (binding-original-code binding)))
	  (format t "~%~a is a primitive, you cannot trace it."
		  function-name)
	(progn
	  (if (= level 0)
	      (setq *trace-list* (remove function-name *trace-list*))
	    (push function-name *trace-list*))
	  (setf (binding-trace? binding) level)
	  (when (and (or (= level 2) (>= level 4)) *first-time-trace*)
	    (format t "
Warning: When tracing a function that is called many times in parallel,
and you are using most of your memory, tracing at level 2 or 4 can cause VCODE 
to run out of memory.  If you run out of memory, trace at level 1 or 3.")
	    (setq *first-time-trace* nil))
	  (compile-function binding env))))))

(defun trace-off (environment)
  (declare (special *trace-list*))
  (do* ((bindings (definition-table-bindings environment) (cdr bindings))
	(binding (cdr (car bindings)) (cdr (car bindings))))
       ((null bindings) nil)
       (when (plusp (binding-trace? binding))
	 (setf (binding-trace? binding) 0)
	 (compile-function binding environment)))
  (setq *trace-list* nil))

;;;;;;;;;;;;;;;;;;
;;;; TIMING
;;;;;;;;;;;;;;;;;

(defun time-exp (exp)
  (let* ((start_timer (get-pscode 'start_timer))
	 (stop_timer (get-pscode 'stop_timer))
	 (pline (get-pscode 'print_line))
	 (++ (get-pscode '++))
	 (str (get-pscode '@))
	 (pstring (format nil " seconds for expression: ~a" 
			  (pprint-nesl-string exp 6))))
    `(let (timer-start (,start_timer 0))
       (let (timer-result ,exp)
	 (let (timer-end (,pline (,++ (pair (,str (,stop_timer 0)) ,pstring))))
	   timer-result)))))

(defun time-body (exp)
  (cond ((nesl-constant-p exp) (time-exp exp))
	((symbolp exp) (time-exp exp))
	((listp exp)
	 (cond ((eql (car exp) 'let) 
		`(let (,(first (second exp)) 
		       ,(time-exp (second (second exp))))
		   ,(time-body (third exp))))
	       ((eql (car exp) 'if)
		`(if ,(second exp) 
		     ,(time-body (third exp))
		   ,(time-body (fourth exp))))
	       ((eql (car exp) 'pair)
		`(pair ,(time-body (second exp)) 
			     ,(time-body (third exp))))
	       ((eql (car exp) 'over) (time-exp exp))
	       (t (time-exp exp))))
	(t (nesl-error "Internal error: Invalid expression, ~s." exp))))

(defun time-function (function on?)
  (if (not on?) function
    (make-func
     :name (func-name function) :arguments (func-arguments function)
     :type (func-type function) :body (time-body (func-body function)))))

(defun nesl-time-switch (function-name on? env)
  (declare (special *profile-list* *argument_check*))
  (when (not (symbolp function-name))
    (nesl-error "Profile must be in the format:~%  ~ 
                     set profile <funname> {on,off};"))
  (let ((binding (get-binding-definition function-name env)))
    (if (not binding) (format t "~%~a is not defined." function-name)
      (if (special-body? (func-body (binding-original-code binding)))
	  (format t "~%~a is a primitive, you cannot time it."
		  function-name)
	(progn
	  (if on?  (push function-name *profile-list*)
	    (setq *profile-list* (remove function-name *profile-list*)))
	  (setf (binding-time? binding) on?)
	  (format t "~%Turning timing ~(~a~) for each let binding of ~a."
		  (if (binding-time? binding) 'on 'off) function-name)
	  (when (and on? *argument_check*)
	    (format t "~%WARNING: argument checking is on; ~
                       use \"set arg_check off;\" for better timings."))
	  (compile-function binding env))))))

(defun time-off (environment)
  (declare (special *profile-list*))
  (do* ((bindings (definition-table-bindings environment) (cdr bindings))
	(binding (cdr (car bindings)) (cdr (car bindings))))
       ((null bindings) nil)
       (when (binding-time? binding)
	 (setf (binding-time? binding) nil)
	 (compile-function binding environment)))
  (setq *profile-list* nil))

;;;;;;;;;;;;;;;;;;;;
;;;; Argument Checking
;;;;;;;;;;;;;;;;;;;;

(defun argcheck? (binding)
  (get-keyword :argcheck (func-other (binding-original-code binding)) nil))

(defun argcheck-function (function binding defs)
  (let* ((check (get-keyword :argcheck (func-other function) nil)))
    (if check
	(let ((check-code (typecheck (func-name function) 
				     (func-arguments function) 
				     nil (nesl-read-exp (first check)) 
				     binding nil defs))
	      (message (format nil "~%RUNTIME ERROR: ~a~%" (second check))))
	  (make-func
	   :name (func-name function) :arguments (func-arguments function)
	   :type (func-type function) :other (func-other function)
	   :body
	   `(let (arg-check (if ,check-code
				(let (x (,(get-pscode 'print_string) 
					 ,message))
				  (let (y (,(get-pscode 'prim-exit) 0))
				    t))
			      f))
	      ,(func-body function))))
      function)))

(defun checkall (environment)
  (do* ((bindings (definition-table-bindings environment) (cdr bindings))
	(binding (cdr (car bindings)) (cdr (car bindings))))
       ((null bindings) nil)
       (when (argcheck? binding)
	 (compile-function binding environment))))
