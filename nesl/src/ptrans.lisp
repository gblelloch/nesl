;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

;; Returns all the variables in a binding expression.
;; For example (bindvars-exp '(pair (pair x y) z))
;; will return (x y z).
(defun bindvars-exp (varlist)
  (if (atom varlist)
      (list varlist)
    (mapcan 'bindvars-exp (cdr varlist))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; the following ext functions find all the free variables in a body
;;;;;;;;;;;;;;;;;;;;;;;;

(defun ext-let-or-over (exp env)
  (nconc (ext-exp (second (second exp)) env)
	 (ext-exp (third exp)
		  (nconc (bindvars-exp (first (second exp)))
			 env))))

(defun ext-func (exp env)
  (let ((func-ext (ext-exp (first exp) env))
	(arg-ext (ext-exp (second exp) env)))
    (nconc func-ext arg-ext)))

(defun ext-pair (exp env)
  (append (ext-exp (second exp) env) (ext-exp (third exp) env)))

(defun ext-symbol (exp env)
  (if (not (find exp env)) (list exp) nil))

(defun ext-if (exp env)
  (append (ext-exp (second exp) env)
	  (ext-exp (third exp) env) 
	  (ext-exp (fourth exp) env)))

(defun ext-exp (exp env)
  (cond ((nesl-constant-p exp) nil)
	((symbolp exp)
	 (ext-symbol exp env))
	((pscode-p exp) nil)
	((listp exp)
	 (cond ((eql (car exp) 'if)
		(ext-if exp env))
	       ((eql (car exp) 'let)
		(ext-let-or-over exp env))
	       ((eql (car exp) 'over)
		(ext-let-or-over exp env))
	       ((eql (car exp) 'pair)
		(ext-pair exp env))
	       ((eql (car exp) 'closure)
		(ext-pair exp env))
	       (t (ext-func exp env))))
	(t (nesl-error "Invalid expression, ~s." exp))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; the following simp function find if an expression just contains pairs
;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-exp (exp)
  (cond ((nesl-constant-p exp) t)
	((symbolp exp) t)
	((pscode-p exp) t)
	((listp exp)
	 (cond ((eql (car exp) 'if) nil)
	       ((eql (car exp) 'let) nil)
	       ((eql (car exp) 'over) nil)
	       ((eql (car exp) 'pair) 
		(and (simp-exp (second exp)) (simp-exp (third exp))))
	       ((eql (car exp) 'closure) 
		(simp-exp (third exp)))
	       (t nil)))
	(t (nesl-error "Invalid expression, ~s." exp))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; the following conv functions do the flattening of nested parallelism
;;;;;;;;;;;;;;;;;;;;;;;;

(defun conv-over (exp pflag env)
  (let* ((binding (second exp))
	 (body (conv-exp (third exp) t env))
	 (free-body (remove-duplicates
		     (ext-exp (third exp) (bindvars-exp (first binding))))))
    `(let ((vector (pair segdes ,(first binding)))
	   ,(conv-exp (second binding) pflag env))
       ,(zip-let-bindings 
	 (mapcar (if pflag
		     #'(lambda (a) 
			 `(,a ((parallel . ,(get-pscode 'prim-dist))
			       (pair (pair ,a segdes)
					   seg_len))))
		   #'(lambda (a) `(,a (,(get-pscode 'prim-dist )
				       (pair ,a segdes)))))
		 free-body)
	 `(let (seg_len ,(if pflag 
			     `(,(get-pscode 'make-segdes)
			       (,(get-pscode 'prim-+-reduce)
				(pair 
				 (,(get-pscode 'prim-seg-lengths)segdes) 
				 seg_len)))
			   'segdes))
	    (,(get-pscode 'vector)
	     (pair segdes ,body)))))))

(defun conv-let (expression pflag env)
  `(let (,(first (second expression)) 
	 ,(conv-exp (second (second expression)) pflag env))
     ,(conv-exp (third expression) pflag env)))

(defun conv-pair (exp pflag env)
  (list (first exp)
	(conv-exp (second exp) pflag env)
	(conv-exp (third exp) pflag env)))

(defun pname (name) (cons 'parallel name))

(defun conv-left-exp (expression pflag env)
  (if (and (pscode-p expression)
	   (is-function-type? (code-type (pscode-serial expression))))
      expression
    (conv-exp expression pflag env)))
      
(defun conv-func (expression pflag env)
  ;;; NOTE THAT THIS HAS TO BE FIXED AT SOME POINT
  ;;; SO THAT THE FUN IS CALLED WITH PFLAG
  (let ((fun (conv-left-exp (first expression) pflag env))
	(arg (conv-exp (second expression) pflag env)))
    (if pflag
	`(,(pname fun) (pair ,arg seg_len))
      `(,fun ,arg))))

(defun nconst (a) (coerce-nesl-constant a))
(defun ps= (a b) `(,(get-pscode '=) (pair ,a ,b)))
(defun vect (segdes vals)  `(,(get-pscode 'vector) (pair ,segdes ,vals)))
(defun iseq (len) 
  `(,(get-pscode 'prim-iseq) (pair ,(nconst 0) (pair ,(nconst 1) ,len))))
(defun pack-i (idx flag segdes)
  `(,(get-pscode 'pack_scalar) 
    (,(get-pscode 'vector) (pair ,segdes (pair ,idx ,flag)))))
(defun pack-ni (idx flag segdes)
  `(,(get-pscode 'pack_scalar) 
    (,(get-pscode 'vector) (pair ,segdes (pair ,idx (,(get-pscode 'not)
						     ,flag))))))
(defun geti (vals idx) `(,(get-pscode '->) (pair ,vals ,idx)))

(defun vselect (cond then else) 
  `(,(get-pscode 'vselect)
    (pair ,cond (pair ,then (pair ,else seg_len)))))

(defun puti (vals idx dest) 
  `(let ((vector (pair junk-segdes result)) 
	 (,(get-pscode 'put) (pair ,vals (pair ,idx ,(vect 'seg_len dest)))))
     result))

(defun cond-exec (vars idx body)
  (if (null vars)
      `(let ((vector (pair seg_len junk-val)) ,idx) ,(vect 'seg_len body))
    (let ((var (car vars)))
      `(let ((vector (pair junk-segdes ,var)) ,(geti (vect 'seg_len var) idx))
	 ,(cond-exec (cdr vars) idx body)))))

(defun cond-merge (v1 i1 v2 i2)
  `(let ((vector (pair junk-segdes result)) 
	 (,(get-pscode 'join) (pair ,v1 (pair ,i1 (pair ,v2 ,i2)))))
     result))

(defun conv-if (expression pflag env)
  (let ((cond (conv-exp (second expression) pflag env))
	(then (conv-exp (third expression) pflag env))
	(else (conv-exp (fourth expression) pflag env)))
    (if (not pflag)
	`(if ,cond ,then ,else)
      (let ((then-vars (remove-duplicates (ext-exp (third expression) nil)))
	    (else-vars (remove-duplicates (ext-exp (fourth expression) nil)))
	    (then-simple? (simp-exp (third expression)))
	    (else-simple? (simp-exp (fourth expression))))
	`(let (cond ,cond)
	   (let (total-count (,(get-pscode 'prim-seg-lengths) seg_len))
	     (let (true-count (,(get-pscode 'prim-count)
			       (pair cond seg_len)))
	       (let (false-count (,(get-pscode '-)
				  (pair total-count true-count)))
		 (if ,(ps= 'total-count (nconst 0))
		     ;; Hack to fix bug that Andrzej Filinski found.
		     ;; This should return an empty vector of the appropriate
		     ;; type, but unfortunately we don't know the type here.
		     then-body-hack
		   (if ,(ps= 'true-count 'total-count)
		       ,then
		     (if ,(ps= 'false-count 'total-count)
			 ,else
		       ,(if then-simple?
			    (if else-simple?
				(vselect 'cond then else)
			      `(let (idx ,(iseq 'seg_len))
				 (let (else-idx ,(pack-ni 'idx 'cond 'seg_len))
				   ,(puti (cond-exec else-vars 'else-idx else)
					  'else-idx then))))
			  (if else-simple?
			      `(let (idx ,(iseq 'seg_len))
				 (let (then-idx ,(pack-i 'idx 'cond 'seg_len))
				   ,(puti (cond-exec then-vars 'then-idx then)
					  'then-idx else)))
			    `(let (idx ,(iseq 'seg_len))
			       (let (then-idx ,(pack-i 'idx 'cond 'seg_len))
				 (let (else-idx ,(pack-ni 'idx 'cond 'seg_len))
				   ,(cond-merge
				     (cond-exec then-vars 'then-idx then)
				     'then-idx
				     (cond-exec else-vars 'else-idx else)
				     'else-idx)))))))))))))))))

;; If we have a constant expression (no free variables), and pflag is true,
;; then we have to distribute the expression across all parallel 
;; occurences.

(defun conv-constant (constant pflag env)
  (if pflag
  `(,(get-pscode 'prim-dist)
    (pair ,(conv-exp constant nil env) seg_len))
  (coerce-nesl-constant constant)))

;;(defun is-const-exp (expression)
;;  (and (listp expression)
;;       (pscode-p (car expression))
;;       (eql (code-name (pscode-serial (car expression))) 'vector)
;;       (not (ext-exp expression nil))))

(defun is-const-exp (expression)
  (not (ext-exp expression nil)))

(defun conv-exp (expression pflag env)
  (cond (;; this is true if there are no free variables in the body
	 (and pflag (is-const-exp expression))
	 (conv-constant expression pflag env))
	((nesl-constant-p expression) 
	 (conv-constant expression pflag env))
	((symbolp expression) expression)
	((pscode-p expression) expression)
	((listp expression)
	 (cond ((eql (car expression) 'let)
		(conv-let expression pflag env))
	       ((eql (car expression) 'over)
		(conv-over expression pflag env))
	       ((eql (car expression) 'if)
		(conv-if expression pflag env))
	       ((eql (car expression) 'pair)
		(conv-pair expression pflag env))
	       ((eql (car expression) 'closure)
		(conv-pair expression pflag env))
	       ;;((eql (car expression) 'sif)
	       ;;`(if ,@(conv-list (cdr expression) pflag env)))
	       (t (conv-func expression pflag env))))
	(t (nesl-error "Invalid expression, ~s." expression))))

;; Can get rid of this once everything is pre type-checked.
(defun conv-body (variables body pflag)
  (when (and pflag (ext-exp body (bindvars-exp (first variables))))
    (nesl-error "Internal Error: unbound variables: ~a."
		(ext-exp body (bindvars-exp (first variables)))))
  (conv-exp body pflag nil))

(defun conv-type (type pflag)
  (if (and pflag (listp (car type)) (eql (caar type) 'function))
      (let ((type-exp (car type))
	    (context (cdr type)))
	(cons `(function ,(second type-exp) ,(third type-exp)
			 (pair ,(fourth type-exp) segdes))
	      context))
    type))

(defun conv-names (names pflag)
  (if pflag
      (list (first names) `(pair ,(second names) seg_len))
    names))
