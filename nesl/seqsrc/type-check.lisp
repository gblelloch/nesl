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

(defun is-defrec? (exp)
  (and (consp exp) (eql (car exp) 'defrec)))

(defun type-<= (type1 type2 definitions)
  (declare (ignore definitions))
  (cond ((eql type2 'any) t)
	((eql type2 'number) (member type1 '(number int float)))
	((eql type2 'logical) (member type1 '(logical int bool)))
	((eql type2 'ordinal) (member type1 '(ordinal number int float char)))
	((eql type2 'scalar) (member type1 '(scalar ordinal number logical int 
						     float char bool)))
	(t nil)))

(defun variable-type? (type definitions)
  (declare (ignore definitions))
  (member type '(any number logical ordinal scalar)))

(defun is-prim-type? (type)
  (member type '(int float char bool)))

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
  ;;(format t "val: ~a~%newval: ~a~%eql: ~a~%" val newval (eq val newval))
  (let ((valtype (var-pointer val))
	(newvaltype (if (is-variable? newval)
			(var-pointer newval)
		      newval)))
    (or (check-inclusion val newval definitions)
	(if (type-<= newvaltype valtype definitions)
	    (progn
	      (setf (var-pointer val) newval)
	      nil)
	  (if (type-<= valtype newvaltype definitions)
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
;  (format t "type1: ~a~%type2: ~a~%~%" type1 type2)
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

(defun instantiate-type (type)
  (do ((vars (cdr type) (cdr vars))
       (spec (car type)))
      ((not vars) spec)
      (setq spec (subst (make-variable (second (car vars)))
			(first (car vars))
			spec))))

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
;;  (format t "func-exp: ~a~%" expression)
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
;;	(format t "match-type ~a, func-type ~a~%" match-type func-type)   
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
;;	(format t "~a, ~a~%" arg-type func-type)
;;	(format t "func-body: ~a~%arg-body: ~a~%match-type: ~a~%" 
;;		func-body arg-body match-type)
	(values (list func-body arg-body)
		(third match-type))))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK PATTERNS 
;;;;;;;;;;;;;;;;;;;;;;;

;;; Used in pattern matching with a WITH, OVER or DEFOP.

(defun nondefinable-constant? (a definitions)
  (declare (ignore definitions))
  (member a '(t f time)))

(defun check-valid-varname (name definitions)
  (when (nondefinable-constant? name definitions)
    (nesl-error "The symbol ~a is a special symbol, it cannot be used as a ~
                 variable." name))
  (when (not (symbolp name))
    (nesl-error "The value ~a is not a valid variable name." 
		(pprint-nesl-short-string name))))

(defun bind-typecheck-rec (expression environment)
  (when (or (eql expression 'seq_dist) (eql expression 'make_sequence))
    (nesl-error "In binding: ~a~%~
                 square brackets cannot be used for pattern matching."
		(pprint-nesl-string expression 2)))
  (multiple-value-bind (bindings arg-type)
    (bind-typecheck-exp (second expression) environment)
    ;; Modified slightly (4/16/95) -- Guy
    (let ((typedef (get-type-def (first expression) environment)))
      (when (not typedef)
	(nesl-error "In binding: ~a~%record type ~a is undefined." 
		    (pprint-nesl-string expression 2)
		    (car expression)))
      (let ((datatype-type (instantiate-type typedef)))
	(when (unify arg-type (fourth datatype-type) environment)
	  (nesl-error "In the pattern~a~%~
                   the arguments don't match the datatype definition."
		      (pprint-nesl-string expression 2)))
	(values bindings (third datatype-type))))))

(defun bind-typecheck-pair (expression environment)
  (multiple-value-bind (bindings1 type1)
    (bind-typecheck-exp (second expression) environment)
    (multiple-value-bind (bindings2 type2)
      (bind-typecheck-exp (third expression) environment)
      (values (append bindings2 bindings1) (list 'pair type1 type2)))))

(defun bind-typecheck-exp (expression environment)
  (cond ((atom expression) 
	 (let ((variable (make-variable 'any)))
	   (values (list (cons expression variable)) 
		   variable)))
	((eql (car expression) 'pair)
	 (bind-typecheck-pair expression environment))
	(t
	 (bind-typecheck-rec expression environment))))

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
	      2 nil -1) 
	     (print-type bind-type (second type-env)))))
	(multiple-value-bind (body-code body-type)
	  (typecheck-exp body-exp (cons (append pattern-binds (car type-env))
					(cdr type-env)))
	  (values (list (car exp) (list pattern-exp bind-code) body-code)
		  (if over? (list 'vector body-type) body-type)))))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK A LOCAL FUNCTION DEFINITION OR CLOSURE (=>, or closure)
;;;;;;;;;;;;;;;;;;;;;;;

;; THIS NEEDS TO BE FIXED UP
(defun typecheck-lambda (name argument body type type-env)
  (multiple-value-bind (bindings source-type)
    (bind-typecheck-exp argument (second type-env))
    (let* (
	   ;; First generate the function type 
	   (full-type (list 'function nil (make-variable 'any) source-type))

	   ;; Add binding for recursive calls -- if letrec
	   (bindings (append bindings (first type-env)))
	   (bindings (if name (cons (cons name full-type) bindings)
		       bindings)))

      ;; Check that type matches the supplied type 
      (when (and type (unify-list full-type (instantiate-type type)
				  (second type-env)))
	(nesl-error "Supplied type does not match arguments."))

      ;; Typecheck the body 
      (multiple-value-bind (new-body result-type)
	(typecheck-exp body (list bindings (second type-env)))
	(declare (ignore new-body))
	(when (unify result-type (third full-type) (second type-env))
	  (nesl-error "Inferred result type~% ~a~%~
                either does not match a user supplied result type,~%~
                or does not match the result type in a recursive call:~% ~a."
		      (print-type result-type (second type-env))
		      (print-type (third full-type) (second type-env))))
	(values nil full-type)))))

(defun typecheck-fundef (exp type-env)
  (typecheck-lambda nil (second exp) (third exp) nil type-env))

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

    ;; first look for variable within local variables
    (if binding (values expression binding)

      ;; now look for it in the predefined functions
      (let ((binding (get-def expression (second type-env))))
	(values expression (instantiate-type (binding-type binding)))))))


(defun user-defined-type? (type-name defs var-type?)
;;  (format t "defs ~a " (length defs))
  (if (null defs) nil
    (let ((first-def (first defs)))
      (if (eql type-name (binding-name first-def))
	  (if (binding-datatype first-def) 
	      (if var-type? (not (null (second (binding-type first-def))))
		(null (second (binding-type first-def)))) nil)
	(user-defined-type? type-name (cdr defs) var-type?)))))


(defun user-defined-type-vars (type-name defs)
;;  (format t "defs ~a " (length defs))
  (if (null defs) (values nil nil)
    (let ((first-def (first defs)))
      (if (eql type-name (binding-name first-def))
	  (if (binding-datatype first-def) 
	      (values t (cdr (binding-type first-def)))
	    (values nil nil))
	(user-defined-type-vars type-name (cdr defs))))))




(defun parse-simple-type (type defs base-type) 
  (cond
   ((atom type) 
    (if (member type '(int char bool float)) type
      (if (and (not base-type)
	       (user-defined-type? type defs nil)) (list type) 
	(nesl-error "No type definition for ~a." type))))
;		    (pprint-nesl-short-string type)))))
   (t
    (cond
     ((eql (car type) 'pair) (list 'pair (parse-simple-type (second type) defs
							    base-type)
			      (parse-simple-type (third type) defs base-type)))
     ((eql (car type) 'vector) (list 'vector (parse-simple-type (second type) 
							    defs base-type)))
     (t (nesl-error "No type definition for ~a." 
		    (pprint-nesl-short-string type)))))))


(defun search-type-binding (var type-bindings)
  (if (null type-bindings) nil
    (let ((first-binding (first type-bindings)))
      (if (eql var (first first-binding)) first-binding
	(search-type-binding var (cdr type-bindings))))))


(defun check-var-list (var-list type-bindings)
  (if (null var-list) t
    (let* ((var (first var-list))
	   (type-bind (search-type-binding var type-bindings)))
      (if (not type-bind) (nesl-error "No type definition for ~a." var)
	(if (not (variable-type? (second type-bind) nil)) 
	    (nesl-error "The type ~a is not a valid type-class." 
			(second type-bind))
	  (check-var-list (cdr var-list) type-bindings))))))


(defun listify (type)
  (cond 
   ((atom type) (list type))
   (t (if (eql (first type) 'pair) (append (listify (second type))
					   (listify (third type)))
	(cons (first type) (listify (cdr type)))))))


(defun check-each-arg (arg-list var-list)
  (if (null arg-list) t 
    (let ((arg (first arg-list))
	  (var (first var-list)))
      (and (is-prim-type? arg) (type-<= arg (second var) nil) 
	   (check-each-arg (cdr arg-list) (cdr var-list))))))


(defun member-type-list (arg-list var-list)
  (if (not (eql (length var-list) (length arg-list))) nil
    (check-each-arg arg-list var-list)))


(defun parse-non-simple-type (type defs var-list)
  (cond
   ((atom type) 
    (if (is-prim-type? type) (values type var-list)
      (if (user-defined-type? type defs nil) (values (list type) var-list) 
	(values type (cons type var-list)))))
   (t
    (cond
     ((eql (car type) 'pair) 
      (multiple-value-bind (t1 v1) (parse-non-simple-type (second type) defs
							  var-list)
	(multiple-value-bind (t2 v2) (parse-non-simple-type (third type) defs
							    var-list) 
	  (values (list 'pair t1 t2) (append v1 v2)))))
     ((eql (car type) 'vector) 
      (multiple-value-bind (t1 v1) (parse-non-simple-type (second type) defs
							  var-list)
			   (values (list 'vector t1) v1)))
     (t (let ((typename (first type))) 
	  (multiple-value-bind (flag var-list) 
			       (user-defined-type-vars typename defs) 
	     (if (not flag) 
		  (nesl-error "No definition for type ~a." typename)
	       (let ((arg-list (listify (second type))))
		 (if (not (member-type-list arg-list var-list))
		     (nesl-error "Incorrect or incomplete specification for datatype ~a." typename) 
		   (cons typename arg-list)))))))))))
		  


(defun parse-general-type (type  type-bindings defs) 
   (multiple-value-bind (type var-list) (parse-non-simple-type type defs nil)
      (if (check-var-list var-list type-bindings) type nil)))
  

(defun typecheck-const (val defns)
  (typecase val
	    (integer 'int)
	    (double-float 'float)
	    (symbol 'bool)
	    (character 'char)
	    (string '(vector char))
	    (cons 
	     (cond
	      ((eql (first val) 'make-empty-vector)
	       (list 'vector (parse-simple-type (second val) defns nil)))
	      ((eql (first val) 'nesl-run-error) 'any)
	      (t (nesl-error "invalid constant type"))))
	    (t (nesl-error "invalid constant type"))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK AN EXPRESSION
;;;;;;;;;;;;;;;;;;;;;;;

(defun typecheck-exp (expression type-env) 
  (cond ((nesl-constantp expression)
	 (values expression (typecheck-const expression (second type-env))))
	((symbolp expression) 
	 (typecheck-variable expression type-env))
	((listp expression)
	 (cond ((eql (car expression) 'let)
		(typecheck-let-or-over expression type-env))
	       ((eql (car expression) 'over)
		(typecheck-let-or-over expression type-env))
	       ((eql (car expression) 'if)
		(typecheck-if expression type-env))
	       ((eql (car expression) 'pair)
		(typecheck-pair expression type-env))
	       ((eql (car expression) '=>)
		(typecheck-fundef expression type-env))
	       (t
		(typecheck-function expression type-env))))
	(t (nesl-error "Invalid expression, ~s." expression))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE CHECK A FUNCTION (POTENTIALLY RECURSIVE)
;;;;;;;;;;;;;;;;;;;;;;;

(defun typecheck-top-func (func defs)
  (let ((name (func-name func))
	(argument (car (func-arguments func)))
	;; Added listify-full-type (4/16/95) -- Guy
	(type (listify-full-type (func-type func) defs))
	(body (func-body func)))
    (if (or (eql body :stub)
	    (and (listp body) (eql (car body) :primitive)))
	(clean-type (instantiate-type type) defs)
      (multiple-value-bind (code funtype)
	(typecheck-lambda name argument body type (list nil defs))
	(declare (ignore code))
	(clean-type funtype defs)))))


(defun typecheck-top-exp (exp defs)
  (multiple-value-bind (new-body result-type)
    (typecheck-exp exp (list nil defs nil))
    (declare (ignore new-body))
    (clean-type result-type defs)))

(defun typecheck-assign (exp defs)
  (multiple-value-bind (bindings bind-type)
    (bind-typecheck-exp (second exp) defs)
    (multiple-value-bind (new-body exp-type)
      (typecheck-exp (third exp) (list nil defs nil))
      (declare (ignore bindings new-body))
      (when (unify bind-type exp-type defs)
	(nesl-error "Expression type does not match bindings."))
      (clean-type exp-type defs))))


;(defun typecheck-defrec (exp defs)
;  (parse-simple-type (third exp) defs))

;; this will typecheck a toplevel expression or a function definition
(defun typecheck-toplevel (exp defs)
  (cond ((func-p exp) 
	 (typecheck-top-func exp defs))
	((is-defrec? exp)
	 (typecheck-defrec exp defs))
	((and (listp exp) (eql (car exp) 'assign))
	 (typecheck-assign exp defs))
	(t (typecheck-top-exp exp defs))))


