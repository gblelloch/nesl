;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MUTABLE TYPE VARIABLES FOR USE IN TYPE CHECKING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct var pointer)

(defun make-variable (type)
  (make-var :pointer type))

(defun is-variable? (v) (var-p v))

(defun is-bound-variable? (v definitions)
  (and (var-p v) (not (variable-type? (var-pointer v) definitions))))

(defun is-unbound-variable? (v definitions)
  (and (var-p v) (variable-type? (var-pointer v) definitions)))

(defun is-function? (type)
  (or (and (listp type) (eql (car type) 'function))
      (and (var-p type) (or (eql (var-pointer type) 'any)
			    (is-function? (var-pointer type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETTING MUTABLE TYPE VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-inclusion-list (var typelist definitions)
  (if typelist
      (or (check-inclusion var (car typelist) definitions)
	  (check-inclusion-list var (cdr typelist) definitions))
    nil))

(defun check-inclusion (var type definitions)
  (cond ((is-bound-variable? type definitions)
	 (check-inclusion var (var-pointer type) definitions))
	((is-variable? type)
	 (eq var type))
	((atom type) nil)
	(t (check-inclusion-list var (cdr type) definitions))))

(defun set-variable (val newval definitions)
  ;;(format t "val: ~a~%newval: ~a~%~%" val newval)
  (let ((valtype (var-pointer val))
	(newvaltype (if (is-variable? newval)
			(var-pointer newval)
		      newval)))
    (or (check-inclusion val newval definitions)
	(if (type-< newvaltype valtype definitions)
	    (progn
	      (setf (var-pointer val) newval)
	      nil)
	  (if (type-< valtype newvaltype definitions)
	      (progn
		(setf (var-pointer val) newval)
		(setf (var-pointer newval) valtype)
		nil)
	    :error1)))))

;;;;;;;;;;;;;;;;;;;;;
;;; STRIPPING OFF THE MUTABLE TYPE VARIABLES FROM A TYPE EXPRESSION
;;;;;;;;;;;;;;;;;;;;;

;;; Used after the completion of type checking.

(defparameter *typevarlist* 
  '(a b c d e))

(defun typevarnames (n)
  (if (> n (length *typevarlist*))
      (do* ((i 0 (+ i 1))
	   (name (intern (format nil "A~d" i)) 
		 (intern (format nil "A~d" i)))
	   (l (list name) (cons name l)))
	  ((= i n) (reverse l)))
    (subseq *typevarlist* 0 n)))

(defun clean-type-list (type definitions)
  (if type
      (let ((thead (clean-type-r (car type) definitions))
	    (ttail (clean-type-list (cdr type) definitions)))
	(cons (cons (car thead) (car ttail))
	      (union (cdr thead) (cdr ttail))))
    nil))

(defun clean-type-r (type definitions)
  (cond ((is-unbound-variable? type definitions)
	 (cons type (list type)))
	((is-variable? type)
	 (clean-type-r (var-pointer type) definitions))
	((atom type)
	 (cons type nil))
	(t
	 (clean-type-list type definitions))))

(defun clean-type (type definitions)
  (let* ((ctype (clean-type-r type definitions))
	 (class (mapcar #'var-pointer (cdr ctype)))
	 (new-vars (typevarnames (length (cdr ctype)))))
    (do* ((ftype (car ctype) (subst (car nvars) (car vars) ftype))
	  (vars (cdr ctype) (cdr vars))
	  (nvars new-vars (cdr nvars)))
	 ((not vars) (cons ftype (mapcar #'list new-vars class))))))

(defun print-type (type definitions)
  (let ((clean-type (clean-type type definitions)))
    (pprint-oneline-string (cnesl-full-type clean-type t))))

(defun print-fun-arg-type (type definitions)
  (let ((clean-type (clean-type type definitions)))
    (pprint-oneline-string 
     (cnesl-full-type (cons (fourth (car clean-type)) (cdr clean-type))
		      t))))

;;;;;;;;;;;;;;;;;;;;;
;;; UNIFICATION
;;;;;;;;;;;;;;;;;;;;;

(defun unify-list (type1 type2 definitions)
  (if type1
      (if type2
	  (or (unify (car type1) (car type2) definitions)
	      (unify-list (cdr type1) (cdr type2) definitions))
	:error2)
    (if type2 :error2 nil)))

(defun unify (type1 type2 definitions)
  ;;(format t "type1: ~a~%type2: ~a~%~%" type1 type2)
  (cond ((eql type1 type2) nil)
	((is-bound-variable? type1 definitions)
	 (unify (var-pointer type1) type2 definitions))
	((is-bound-variable? type2 definitions)
	 (unify type1 (var-pointer type2) definitions))
	((is-variable? type1)
	 (set-variable type1 type2 definitions))
	((is-variable? type2)
	 (set-variable type2 type1 definitions))
	((or (atom type1) (atom type2))
	 :error3)
	;; In this final case both types are lists (records)
	;; and the first element must be atomic and equal
	(t (if (and (atom (car type1)) (atom (car type2))
		    (eql (car type1) (car type2)))
	       (unify-list (cdr type1) (cdr type2) definitions)
	     :error4))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONVERT EXPLICIT TYPE TO IMPLICIT TYPE
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instantiate-fun-type (type)
  (do ((vars (cdr type) (cdr vars))
       (spec (car type)))
      ((not vars) spec)
      (setq spec (subst (make-variable (second (car vars)))
			(first (car vars))
			spec))))

(defun instantiate-nil-type-list (type)
  (if (null type) nil
    (cons (instantiate-nil-type (car type))
	  (instantiate-nil-type-list (cdr type)))))

;; converts all nil's into any's
(defun instantiate-nil-type (type)
  (cond ((null type) (make-variable 'any))
	((listp type) (instantiate-nil-type-list type))
	(t type)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIND SPECIFIC TYPES
;;;;;;;;;;;;;;;;;;;;;;;;;

;; This functions is used by trans.lisp in the final generation of code
;; to determine specific result types
(defun get-instantiated-function-type (fundef arg-type definitions) 
  (let ((function-type (instantiate-fun-type (code-type fundef)))
	(arg-type (instantiate-nil-type arg-type)))
    (when (unify arg-type (fourth function-type) definitions)
      (nesl-error "INTERNAL ERROR in function call ~a,~%~
               inferred argument types don't match function specification.~%  ~
               Argument types: ~a~%  ~
               Function spec:  ~a"
		  (code-name fundef)
		  (print-type arg-type definitions)
		  (print-fun-arg-type function-type definitions)))
    (third (car (clean-type-r function-type definitions)))))

;;; used by base-typecase to determine the type of a typevar
(defun foo-type (funtype var argtypes definitions)
  (let ((inst (instantiate-fun-type (cons (cons var (car funtype)) 
					  (cdr funtype)))))
    (unify-list argtypes (cddddr inst) definitions)
    (car (clean-type (car inst) definitions))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK A PAIR
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun typecheck-pair (expression type-env)
  (multiple-value-bind (first-body first-type)
    (typecheck-exp (second expression) type-env)
    (multiple-value-bind (second-body second-type)
      (typecheck-exp (third expression) type-env)
      (values (list 'pair first-body second-body)
	      (list 'pair first-type second-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK A FUNCTION CALL
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun typecheck-function (expression type-env)
  (multiple-value-bind (arg-body arg-type)
    (typecheck-exp (second expression) type-env)
    (let ((definitions (second type-env))
	  (match-type (list 'function (make-variable 'any) 
			    (make-variable 'any) arg-type)))
      (multiple-value-bind (func-body func-type)
	(typecheck-exp (first expression) type-env)
	(when (not (is-function? func-type))
	  (nesl-error "In expression ~a,~%~a is not a function type.~%~
                   It is of type: ~a"
		      (pprint-nesl-string expression 2)
		      (pprint-nesl-short-string (car expression))
		      (print-type func-type definitions)))
	(when (unify match-type func-type definitions)
	  (multiple-value-bind (func-body func-type)
	    (typecheck-exp (first expression) type-env)
	    (declare (ignore func-body))
	    (nesl-error "For function ~a in expression~a~%~
               inferred argument types don't match function specification.~%  ~
               Argument types: ~a~%  ~
               Function types: ~a"
			(pprint-nesl-short-string (car expression))
			(pprint-nesl-string expression 2)
			(print-type arg-type definitions)
			(print-fun-arg-type func-type definitions))))
	(values (list func-body arg-body)
		(third match-type))))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK PATTERNS 
;;;;;;;;;;;;;;;;;;;;;;;

;;; Used in pattern matching with a WITH, OVER or DEFOP.

(defun check-valid-varname (name definitions)
  (when (nondefinable-constant? name definitions)
    (nesl-error "The symbol ~a is a special symbol, it cannot be used as a ~
                 variable." name))
  (when (not (symbolp name))
    (nesl-error "The value ~a is not a valid variable name." 
		(pprint-nesl-short-string name))))

(defun bind-typecheck-list (list environment)
  (if list
      (multiple-value-bind (head-binds head-type)
	(bind-typecheck-exp (car list) environment)
	(multiple-value-bind (rest-binds rest-type)
	  (bind-typecheck-list (cdr list) environment)
	  (values (append head-binds rest-binds)
		  (cons head-type rest-type))))
    (values nil nil)))

(defun bind-typecheck-rec (expression environment)
  (when (and (pscode-p (car expression))
	     (let ((name (code-name (pscode-serial (car expression)))))
	       (or (eql name 'seq_dist) (eql name 'make_sequence))))
    (nesl-error "In binding: ~a~%~
                 square brackets cannot be used for pattern matching."
		(pprint-nesl-string expression 2)))
  (multiple-value-bind (bindings arg-type)
    (bind-typecheck-list (cdr expression) environment)
    (let* ((typedef (get-typedef (car expression) environment))
	   (typedef-type
	    (if typedef (instantiate-fun-type (typedef-type typedef))
	      (nesl-error "In binding: ~a~%record type ~a is undefined." 
			  (pprint-nesl-string expression 2)
			  (if (pscode-p (car expression))
			      (code-name (pscode-serial (car expression)))
			    (car expression))))))
      (when (unify-list arg-type (cdr typedef-type) environment)
	(nesl-error "In the pattern~a~%~
                   the arguments don't match the datatype definition."
		    (pprint-nesl-string expression 2)))
      (values bindings (car typedef-type)))))

(defun bind-typecheck-exp (expression environment)
  (if (atom expression) 
      (let ((variable (make-variable 'any)))
	(values (list (cons expression variable)) 
		variable))
    (bind-typecheck-rec expression environment)))

(defun bind-typecheck (expression environment)
  (multiple-value-bind (binds type)
    (bind-typecheck-exp expression environment)
    (do* ((head (car binds) (car rest))
	  (rest (cdr binds) (cdr rest))
	  (check (check-valid-varname (car head) environment)
		 (check-valid-varname (car head) environment)))
	 ((not rest))
	 (when (find (car head) rest :key #'car)
	   (nesl-error "The variable: ~(~a~)~%is repeated in the pattern of a ~
                       let binding, or an apply-to-each." 
		       (car head))))
    (values binds type)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK EITHER A "let" OR "over" FORM
;;;;;;;;;;;;;;;;;;;;;;;

(defun typecheck-let-or-over (exp type-env)
  (let ((pattern-exp (first (second exp)))
	(bind-exp (second (second exp)))
	(body-exp (third exp))
	(over? (eql (car exp) 'over)))
    (multiple-value-bind (pattern-binds pattern-type)
      (bind-typecheck pattern-exp (second type-env))
      (multiple-value-bind (bind-code bind-type)
	(typecheck-exp bind-exp type-env)
	(let ((match-type (if over? (list 'vector pattern-type)
			    pattern-type)))
	  (when (unify match-type bind-type (second type-env))
	    (nesl-error 
	     "In the binding of ~a construct~a~%~
                the inferred type on the right hand side~%  ~a~%~
                does not match the pattern on the left."
	     (if over? "an APPLY-TO-EACH" "a LET")
	     (pprint-nesl-string-rec 
	      (if over? (cnesl-over-binding (second exp))
		(cnesl-with-binding (second exp)))
	      2 -1) 
	     (print-type bind-type (second type-env)))))
	(multiple-value-bind (body-code body-type)
	  (typecheck-exp body-exp (cons (append pattern-binds (car type-env))
					(cdr type-env)))
	  (values (list (car exp) (list pattern-exp bind-code) body-code)
		  (if over? (list 'vector body-type) body-type)))))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK A LOCAL FUNCTION DEFINITION OR CLOSURE (=>, or closure)
;;;;;;;;;;;;;;;;;;;;;;;

(defun typecheck-fundef (exp type-env)
  (let ((pattern-exp (second exp))
	(body-exp (third exp)))
    (multiple-value-bind (pattern-binds pattern-type)
      (bind-typecheck-exp pattern-exp (second type-env))
      (multiple-value-bind (body-code body-type)
	(typecheck-exp body-exp (cons (append pattern-binds (car type-env))
				      (cdr type-env)))
	(let* ((free-vars (ext-exp body-code (mapcar 'car pattern-binds)))
	       (free-arg (if free-vars (list-pair-r free-vars) nil))
	       (args (if free-arg
			 (list `pair free-arg pattern-exp)
		       pattern-exp))
	       (bind (function-binding-xx args body-code (second type-env)))
	       (pscode (binding-compiled-code bind)))
	  (if (not free-vars)
	      (values pscode (list 'function bind body-type pattern-type))
	    (multiple-value-bind (ignore free-types)
	      (typecheck-exp free-arg type-env)
	      (declare (ignore ignore))
	      (values (list 'closure pscode free-arg)
		      (list 'function (list 'closure pscode free-types)
			    body-type pattern-type)))))))))

(defun typecheck-closure (expression type-env)
  (multiple-value-bind (arg-exp arg-type)
    (typecheck-exp (third expression) type-env)
    (multiple-value-bind (fun-exp fun-type)
      (typecheck-exp (second expression) type-env)
      (let ((stype (make-variable 'any))
	    (dtype (make-variable 'any)))
	(when (unify fun-type 
		     (list 'function (make-variable 'any) dtype
			   (list 'pair arg-type stype))
		     (second type-env))
	  (nesl-error "oh shit!"))
	(values (list 'closure fun-exp arg-exp)
		(list 'function (list 'closure fun-exp arg-type)
		      dtype stype))))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK AN "if" FORM
;;;;;;;;;;;;;;;;;;;;;;;

(defun typecheck-if (expression type-env)
  (multiple-value-bind (cond-body cond-type)
    (typecheck-exp (second expression) type-env)
    (multiple-value-bind (then-body then-type)
      (typecheck-exp (third expression) type-env)
      (multiple-value-bind (else-body else-type)
	(typecheck-exp (fourth expression) type-env)
	(when (unify cond-type 'bool (second type-env))
	  (nesl-error 
	   "In the IF expression~a~%~
        the first argument must be of type BOOL.  The inferred type is ~a."
	   (pprint-nesl-string expression 2)
	   cond-type))
	(when (unify then-type else-type (second type-env))
	  (nesl-error 
	   "In the IF expression~a~%~
        the two branches must be of the same type.  The inferred types are~%  ~
        THEN type: ~a~%  ELSE type: ~a."
	   (pprint-nesl-string expression 2)
	   (print-type then-type (second type-env)) 
	   (print-type else-type (second type-env))))
	(values (list 'if cond-body then-body else-body) then-type)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK A VARIABLE
;;;;;;;;;;;;;;;;;;;;;;;

(defun typecheck-variable (expression type-env)
  (let ((binding (cdr (assoc expression (car type-env)))))
    (if binding (values expression binding)
      (if (eql expression (first (third type-env)))
	  (values expression ;;xxxx (second (third type-env))
		  (third (third type-env)))
	(let ((binding (get-binding-definition expression (second type-env))))
	  (if binding
	      (values (binding-compiled-code binding)
		      (instantiate-fun-type 
		       (func-type (binding-original-code binding))))
	    (nesl-error "~a is undefined." expression)))))))

;; this should be fixed so that the real type rather than the compiled
;; type is returned -- these would be different for any function with
;; datatypes as arguments.
(defun typecheck-pscode (expression)
  (values expression
	  (instantiate-fun-type (code-type (pscode-serial expression)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK AN EXPRESSION
;;;;;;;;;;;;;;;;;;;;;;;

(defun typecheck-exp (expression type-env)
  (cond ((nesl-constant-p expression)
	 (values (coerce-nesl-constant expression)
		 (nesl-constant-p expression)))
	((symbolp expression) 
	 (typecheck-variable expression type-env))
	((pscode-p expression)
	 (typecheck-pscode expression))
	((listp expression)
	 (cond ((eql (car expression) 'with)
		(nesl-error "With is an invalid form."))
	       ((eql (car expression) 'let)
		(typecheck-let-or-over expression type-env))
	       ((eql (car expression) 'over)
		(typecheck-let-or-over expression type-env))
	       ((eql (car expression) 'if)
		(typecheck-if expression type-env))
	       ((eql (car expression) 'pair)
		(typecheck-pair expression type-env))
	       ((eql (car expression) 'closure)
		(typecheck-closure expression type-env))
	       ((eql (car expression) '=>)
		(typecheck-fundef expression type-env))
	       (t
		(typecheck-function expression type-env))))
	(t (nesl-error "Invalid expression, ~s." expression))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK A FUNCTION (POTENTIALLY RECURSIVE)
;;;;;;;;;;;;;;;;;;;;;;;

(defun make-fun-type (type binding)
  (let ((type (instantiate-fun-type type)))
    (list (first type) binding (third type) (fourth type))))

(defun typecheck-op (name arguments type body binding lazy definitions)
  (multiple-value-bind (bindings source-types)
    (bind-typecheck-list arguments definitions)
    (let* ((full-type (cons 'function 
			    (cons binding (cons (make-variable 'any) 
						source-types))))
	   (check (when (and type 
			     (unify-list full-type 
					 (make-fun-type type binding)
					 definitions))
		    (nesl-error "Supplied type does not match arguments.")))
	   (recurse-bind (list name (binding-compiled-code binding)
			       (if lazy (make-variable 'any)
				 full-type))))
      (declare (ignore check))
      (multiple-value-bind (new-body result-type)
	(typecheck-exp body (list bindings definitions recurse-bind))
	(when (unify result-type (third full-type) definitions)
	  (nesl-error "Inferred result type~% ~a~%~
                either does not match a user supplied result type,~%~
                or does not match the result type in a recursive call:~% ~a."
		      (print-type result-type definitions)
		      (print-type (third full-type) definitions)))
	;;xxxx (values new-body (clean-type full-type definitions))
	(let* ((foo (ext-exp new-body (mapcar 'car bindings)))
	       (body (if foo `(let (,name ,(binding-compiled-code binding)) 
				,new-body)
		       new-body)))
	  (values body (clean-type full-type definitions)))))))

(defun typecheck (name arguments type body bind lazy defs)
  (if (or (eql body :stub)
	  (and (listp body) (eql (car body) :primitive)))
      (values body (clean-type (make-fun-type type bind) defs))
    (typecheck-op name arguments type body bind lazy defs)))

;;; A HACK
(defun fix-typecase (type casevar bind defs)
  (let ((type (clean-type (make-fun-type 
			   (cons (cons casevar (cdar type)) 
				 (cdr type))
			   bind)
			  defs)))
    (values (cons (cons 'function (cdr (car type))) (cdr type))
	    (caar type))))

(defun typecheck-base-typecase (name arguments type body bind defs)
  (multiple-value-bind (type casevar) 
    (fix-typecase type (second body) bind defs)
    (values `(base-typecase
	      ,casevar
	      ,@(mapcar #'(lambda (case)
			    (list (first case)
				  (typecheck name arguments type 
					     (second case) bind t defs)))
			(cddr body)))
	    type)))

(defun typecheck-poly-typecase (name arguments type body bind defs)
  (multiple-value-bind (type casevar) 
    (fix-typecase type (second body) bind defs)
    (values `(poly-typecase
	      ,casevar
	      ,(typecheck name arguments type (third body) bind t defs)
	      ,(typecheck name arguments type (fourth body) bind t defs)
	      ,(typecheck name arguments type 
			  (or (fifth body)
			      (generate-default-pair-code 
			       (cons name arguments)
			       (car type) casevar))
			  bind t defs))
	    type)))

(defun typecheck-const (body type defs)
  (multiple-value-bind (new-body result-type)
    (typecheck-exp body (list nil defs nil))
    (values new-body (or type (clean-type result-type defs)))))

(defun type-check (binding defs)
  (let* ((function (binding-original-code binding))
	 (body (func-body function))
	 (type (func-type function))
	 (arguments (func-arguments function))
	 (name (func-name function)))
    (multiple-value-bind (new-body new-type)
      (cond ((and type (or (null arguments) (not (is-function-type? type))))
	     (typecheck-const body type defs))
	    ((and (listp body) (eql (car body) 'base-typecase))
	     (typecheck-base-typecase name arguments type body binding defs))
	    ((and (listp body) (eql (car body) 'poly-typecase))
	     (typecheck-poly-typecase name arguments type body binding defs))
	    (t
	     (typecheck name arguments type body binding nil defs)))
      (setf (binding-original-code binding)
	    (make-func :name name :arguments arguments
		       :type new-type :body new-body
		       :other (func-other function)))
      binding)))
