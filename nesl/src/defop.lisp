;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp) 

(defun special-body? (body)
  (or (eql body :stub)
      (and (listp body)
	   (or (eql (car body) 'base-typecase)
	       (eql (car body) 'poly-typecase)
	       (eql (car body) :primitive)))))

(defun typecase? (body)
  (and (listp body)
       (or (eql (car body) 'base-typecase)
	   (eql (car body) 'poly-typecase))))

(defun get-keyword (key lst default) 
  (if lst 
      (if (eql key (car lst)) 
	  (second lst) 
	(get-keyword key (cddr lst) default))
    default))

(defun trans-phase1 (variables body pflag defs)
  (if (and (listp body) (eql (car body) :primitive))
      (if pflag 
	  (cons :primitive (cons (list 'POP 1 0) (cdr body)))
	body)
    (free-body (conv-body variables (strip-exp body defs) pflag))))

(defun phase1-trans-body (names body pflag defs)      
  (let ((variables (cdr names)))
    (cond ((null body) (nesl-error "Internal error: No body."))
	  ((eql body :stub) body)
	  ((and (listp body) (eql (car body) 'base-typecase))
	   `(,(first body) ,(second body)
	     ,@(mapcar 
		#'(lambda (a) 
		    (list (first a) 
			  (trans-phase1 variables (second a) pflag defs)))
		(cddr body))))
	  ((and (listp body) (eql (car body) 'poly-typecase))
	   `(,(first body) ,(second body)
	     ,(trans-phase1 variables (third body) pflag defs)
	     ,(trans-phase1 variables (fourth body) pflag defs)
	     ,(trans-phase1 variables (fifth body) pflag defs)))
	  (t (trans-phase1 variables body pflag defs)))))

(defun new-code (code function pflag definitions)
  (let* ((type (func-type function))
	 (name (or (get-keyword :name (func-other function) nil)
		   (func-name function)))
	 (names (list name (strip-pattern-exp (car (func-arguments function))
					      definitions)))
	 (body (func-body function))
	 (newtype (strip-type (conv-type type pflag) definitions))
	 (arguments (cdr (conv-names names pflag)))
	 (translated-code (phase1-trans-body names body pflag definitions)))
    (setf (code-name code) name)
    (setf (code-arguments code) arguments)
    (setf (code-type code) newtype)
    (setf (code-compiled code) translated-code)
    (setf (code-cache code) nil)))

(defun trace-time-argcheck (binding definitions)
  (declare (special *argument_check*))
  (let* ((func (binding-original-code binding))
	 (func (time-function func (binding-time? binding)))
	 (func (trace-function func (binding-trace? binding) definitions))
	 (func (if *argument_check* 
		   (argcheck-function func binding definitions)
		 func)))
    func))

(defun compile-function (binding definitions)
  (declare (special *redefine-default*))
  (let ((code (binding-compiled-code binding))
	(function (trace-time-argcheck binding definitions)))
    (setf (pscode-datatype? code)
	  (get-keyword :datatype (func-other function) nil))
    (setf (binding-redefinable? binding) 
	  (get-keyword :redefine (func-other function) *redefine-default*))
    (new-code (pscode-serial code) function nil definitions)
    (cond ((or (get-keyword :serial (func-other function) nil)
	       (not (parallel-check-op (func-body function))))
	   (setf (pscode-serial-only? code) t))
	  (t
	   (new-code (pscode-parallel code) function t definitions)
	   (setf (pscode-serial-only? code) nil)))
    binding))

(defun type-equal (a b)
  (if (and (listp (car a)) (eql (caar a) 'function))
      (if (and (listp (car b)) (eql (caar b) 'function))
	  (equal (cddar a) (cddar b))
	nil)
    (equal a b)))

(defun get-binding (function definitions)
  (let* ((name (func-name function))
	 (type (func-type function))
	 (main-flag (or (eql name 'main) (eql name 'it)))
	 (olddef (get-binding-definition name definitions)))
    (declare (special *redefine-default*))
    (when (and olddef (not main-flag)
	       (not (eql *redefine-default* 'off))
	       (eql (binding-redefinable? olddef) 'off))
      (nesl-error "~a is built in, it cannot be redefined."
		  name))
    (if (and olddef (type-equal (func-type (binding-original-code olddef))
				type))
	olddef
      (progn
	(when (and olddef (not main-flag))
	  (format t "~%Redefining ~a with a new type.  ~
                     Old calls will not be modified." name))
	nil))))

(defun check-and-compile (function definitions)
  (let* ((binding (type-check (new-binding function definitions) definitions))
	 (oldbind (get-binding (binding-original-code binding) definitions)))
    (if oldbind
	(progn
	  (setf (binding-original-code oldbind) function)
	  ;; need to recheck to update recursive calls and type pointers
	  (type-check oldbind definitions)
	  (compile-function oldbind definitions))
      (progn
	(add-binding-to-environment binding definitions)
	(compile-function binding definitions)))))

(defun add-function-binding (function definitions)
  (declare (special *fully-loaded*))
  (when *fully-loaded*
    (check-valid-varname (func-name function) definitions))
  (let ((binding (check-and-compile function definitions)))
    (values (binding-compiled-code binding)
	    (func-type (binding-original-code binding))
	    (func-name (binding-original-code binding)))))
  
(defun function-binding-xx (arg body definitions)
  (let* ((function (make-func :name (gensym) :arguments (list arg) :body body))
	 (binding (type-check (new-binding function definitions) definitions)))
    (compile-function binding definitions)))

(defun add-global-variable (var-symbol value type definitions &optional other)
  (check-valid-varname var-symbol definitions)
  (check-and-compile (make-func :name var-symbol :type type :body value
				:other other)
		     definitions))

(defun get-defrec-typebindings (list)
  (if (or (null list) (keywordp (car list)))
      nil
    (cons (car list) (get-defrec-typebindings (cdr list)))))

(defun parse-defrec (arguments definitions)
  (let* ((names (first arguments))
	 (typebindings (get-defrec-typebindings (cdr arguments)))
	 (keys (nthcdr (length typebindings) (cdr arguments))))
    (declare (special *redefine-default*))
    (make-defrec names typebindings
		 (get-keyword :documentation keys nil)
		 (get-keyword :redefine keys *redefine-default*)
		 definitions)))

(defun add-type (names type pflag definitions)
  (add-type-def (car (conv-names names pflag) )
		(cons (conv-type (car type) pflag) (cdr type))
		definitions))

(defun make-args (count)
  (do ((i count (- i 1))
       (l nil (cons (intern (format nil "A~a" i)) l)))
      ((zerop i) l)))

(defun make-defrec (names typebindings
			  documentation redefine
			  definitions)
  (declare (ignore redefine))
  (let* ((name (car names))
	 (*current-typedef* name)
	 (simp-types (intern-type-list (cdr names)))
	 (check-valid-type (check-type-list (cons simp-types typebindings)
					    definitions))
	 (full-names (cons name (make-args (length (cdr names)))))
	 (return-type (cons name (mapcar #'first typebindings)))
	 (type (cons (cons return-type simp-types) typebindings))
	 (full-type (cons (cons 'function (cons nil (car type))) (cdr type)))
	 (doc (or documentation
		  (format nil "Constructor for the record type ~a." name)))
	 (function (make-func :name name :type full-type
			      :arguments (cdr full-names) 
			      :body '(:primitive)
			      :other (list :documentation doc :datatype t))))
    (declare (special *current-typedef*) (ignore check-valid-type))
    (add-type full-names type nil definitions)
    (add-function-binding function definitions)))

#| *********************
The following generates code for polymorphic functions on pairs.
|# 

(defun generate-default-pair-code (names funtype typevar)
  ;;(print (list 'here names funtype typevar))
  (multiple-value-bind (binds args1 args2)
    (variable-bind-calls (second names) (fourth funtype) typevar)
    (let ((calls (list (list (car names) args1)
		       (list (car names) args2))))
      (zip-let-bindings
       binds 
       (combine-code calls (third funtype) typevar)))))

(defun variable-bind-calls (args argtypes typevar)
  (if (listp args)
      (multiple-value-bind (bindsleft vleft1 vleft2)
	  (variable-bind-calls (second args) (second argtypes) typevar)
	(multiple-value-bind (bindsright vright1 vright2)
	  (variable-bind-calls (third args) (third argtypes) typevar)
	  (values (append bindsleft bindsright)
		  `(pair ,vleft1 ,vright1)
		  `(pair ,vleft2 ,vright2))))
    (variable-bind-call args argtypes typevar)))

(defun variable-bind-call (arg argtype typevar)
  (cond ((eql argtype typevar)
	 (let ((v1 (gensym)) 
	       (v2 (gensym)))
	   (values `(((pair ,v1 ,v2) ,arg))
		   v1 v2)))
	((and (listp argtype) (eql (car argtype) 'vector)
	      (eql (second argtype) typevar))
	 (let ((v1 (gensym))
	       (v2 (gensym))
	       (seg (gensym)))
	   (values `(((vector 
		       (pair ,seg (pair ,v1 ,v2))) ,arg))
		   `(,(get-pscode 'vector) (pair ,seg ,v1))
		   `(,(get-pscode 'vector) (pair ,seg ,v2)))))
	(t 
	 (values nil arg arg))))

(defun combine-code (calls return-type typevar)
  (cond ((eql return-type typevar)
	 (cons 'pair calls))
	((and (listp return-type) (eql (car return-type) 'vector)
	      (eql (second return-type) typevar))
	 (let ((g1 (gensym)) (g2 (gensym)))
	   `(let ((vector (pair seg ,g1)) ,(first calls))
	      (let ((vector (pair seg ,g2)) ,(second calls))
		(,(get-pscode 'vector)
		 (pair seg (pair ,g1 ,g2)))))))
	(t (nesl-error "Default poly-typecase can't return the type ~a." 
		       return-type))))

