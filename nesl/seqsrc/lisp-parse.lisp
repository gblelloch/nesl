;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

(defun nesl-read-binding (binding over?)
  (when (or (not (listp binding))
	    (not (= (length binding) 2)))
    (nesl-error 
     "In the binding ~a of a ~a form.~%~
            The syntax for a binding is:  (pattern exp)." 
     binding (if over? "V. or OVER" "WITH")))
  `(,(nesl-read-exp (first binding)) ,(nesl-read-exp (second binding))))

(defun nesl-read-bindings (a over?)
  (when (not (listp a))
    (nesl-error
     "In the bindings ~a of a ~a form.~%~
            The syntax for a binding is:  (pattern exp)*." 
     a (if over? "V. or OVER" "WITH")))
  (if (null a) a
    (cons (nesl-read-binding (car a) over?)
	  (nesl-read-bindings (cdr a) over?))))
	    
(defun nesl-read-with (a)
  (when (or (not (= (length a) 3))
	    (not (listp (second a))))
    (nesl-error 
     "In the WITH expression ~%  ~a.~%~
      The syntax for WITH is:  (WITH (expbind*) exp)."
     a))
  (zip-let-bindings (nesl-read-bindings (second a) nil) 
		    (nesl-read-exp (third a))))

(defun vprefixp (symbol)
  (let ((sstring (string symbol)))
    (and (>= (length sstring) 2)
	 (string-equal "v." symbol :end2 2))))

(defun vpostfix (symbol)
  (let ((value (read-from-string (subseq (string symbol) 2))))
    (cond ((numberp value) value)
          ((symbolp value) (intern (string value) (symbol-package symbol)))
          (t (nesl-error "~s is an invalid form" symbol)))))

(defun convert-vexpt-list (list)
  (cond ((null list) (cons nil nil))
	((eql (car list) 'v.)
	 (let ((rest (convert-vexpt-list (cddr list))))
	   (cons (car rest) (cons (cadr list) (cdr rest)))))
	(t
	 (let ((rest (convert-vexpt-list (cdr list)))
	       (this (if (symbolp (car list))
			 (if (vprefixp (car list))
			     (cons nil (vpostfix (car list)))
			   (cons (list (list (car list) (car list))) 
				 (car list)))
		       (let ((sym (gentemp "x")))
			 (cons (list (list sym (car list))) sym)))))
	   (cons (append (car this) (car rest))
		 (cons (cdr this) (cdr rest)))))))

(defun convert-vexp (exp)
  (if (and (symbolp (car exp)) (vprefixp (car exp)))
      (let ((conv (convert-vexpt-list (cdr exp)))
	    (op (vpostfix (car exp))))
	(when (not (car conv))
	  (nesl-error "In expression ~s,~%~
             a v. expression must have at least one argument which is not~%~
             preceeded with a v." exp))
	`(over ,(car conv)
	       ,(cons op (cdr conv))))
    exp))

(defun nesl-read-over (a)
  (when (or (not (= (length a) 3))
	    (not (listp (second a)))
	    (not (> (length (second a)) 0)))
    (nesl-error 
     "In the OVER expression ~%  ~a.~%~
      The syntax for OVER is:  (OVER (expbind+) exp)."
     a))
  `(over ,(zip-over (nesl-read-bindings (second a) t))
	       ,(nesl-read-exp (third a))))

(defun nesl-read-list (a)
  (if (= (length a) 1) (nesl-read-exp (car a))
    `(pair ,(nesl-read-exp (car a)) ,(nesl-read-list (cdr a)))))

(defun nesl-read-func (a)
  (when (not (> (length a) 1))
    (nesl-error 
     "Error in the expression ~%  ~a.~%~
        Function calls require at least one argument."
     a))
  (list (car a) (nesl-read-list (cdr a))))

(defun nesl-read-if (a)
  (when (not (= (length a) 4))
    (nesl-error 
     "Error in the IF expression ~%  ~a.~%~
        The syntax for IF is:  (IF exp exp exp)."
     a))
  `(if ,(nesl-read-exp (second a))
       ,(nesl-read-exp (third a))
     ,(nesl-read-exp (fourth a))))

(defun nesl-read-pair (a)
  (when (not (= (length a) 3))
    (nesl-error 
     "Error in the expression ~%  ~a.~%~
        The syntax for PAIR is:  (PAIR exp exp)."
     a))
  `(pair ,(nesl-read-exp (second a))
	       ,(nesl-read-exp (third a))))
  
(defun nesl-read-exp (exp)
  (cond ((constantp exp) exp)
	((symbolp exp) exp)
	((listp exp)
	 (let ((exp (convert-vexp exp)))
	   (cond ((eql (car exp) 'if)
		  (nesl-read-if exp))
		 ((eql (car exp) 'with)
		  (nesl-read-with exp))
		 ((eql (car exp) 'over)
		  (nesl-read-over exp))
		 ((eql (car exp) 'pair)
		  (nesl-read-pair exp))
		 (t (nesl-read-func exp)))))
	(t (error "In NESL-READ, Invalid expression, ~s." exp))))

(defun nesl-read-next (a)
  (if (and (listp a) (eql (car a) :primitive)) 
      a 
    (nesl-read-exp a)))

(defun nesl-read-top (a)
  (if (listp a)
      (cond ((eql (car a) 'poly-typecase)
	     `(,(first a) ,(second a) 
	       ,(nesl-read-next (third a))
	       ,(nesl-read-next (fourth a))
	       ,@(if (fifth a) (list (nesl-read-next (fifth a))) nil)))
	    ((eql (car a) 'base-typecase)
	     `(,(first a) ,(second a) 
	       ,@(mapcar #'(lambda (a) (list (first a) 
					     (nesl-read-next (second a))))
			 (cddr a))))
	    (t (nesl-read-next a)))
    (nesl-read-exp a)))

(defun defop-syntax-error (a)
  (nesl-error "Bad function definition: (DEFOP ~a ...).~%~
                   The syntax for DEFOP is:~%~%  ~
         (DEFOP (Ident Ident+) [! typespec]~%    exp)~%"
	      a))

(defun nesl-read-type-list (a)
  (if (= (length a) 1) (nesl-read-type-exp (car a))
    `(pair ,(nesl-read-type-exp (car a)) 
		 ,(nesl-read-type-list (cdr a)))))

(defun nesl-read-type-exp (exp)
  (cond ((symbolp exp) exp)
	((listp exp)
	 (cond ((eql (car exp) 'pair)
		`(pair ,(nesl-read-type-exp (second exp))
			     ,(nesl-read-type-exp (third exp))))
	       ((= (length exp) 1) exp)
	       (t `(,(car exp) ,(nesl-read-type-list (cdr exp))))))
	(t (error "In NESL-READ, Invalid expression, ~s." exp))))

(defun nesl-read-prim-defop (a)
  (let* ((names (second a)))
    (when (or (not (listp names))
	      (not (> (length names) 1))
	      (not (symbolp (first names)))
	      (< (length a) 3))
      (defop-syntax-error names))
    (cond ((eql (third a) '! )
	   (when (< (length a) 5) (defop-syntax-error names))
	   (let* ((type (simplify-type (fourth a)))
		  (stype (nesl-read-type-list (cdar type)))
		  (dtype (nesl-read-type-exp (caar type)))
		  (name-args (nesl-read-exp (second a))))
	     (make-func :name (car name-args)
			:arguments (cdr name-args)
			:type (cons (list 'function nil dtype stype) 
				    (cdr type))
;			:body (nesl-read-top (fifth a)) 
; changed to copy body (lambda expression) verbatim for a if_primititive function
 			:body (fifth a)
			:other (cddr (cdddr a)))))
	  (t
	   (let ((name-args (nesl-read-exp (second a))))
	     (make-func :name (car name-args)
			:arguments (cdr name-args)
			:body (nesl-read-top (third a))
			:other (cdddr a)))))))


(defun nesl-read-defop (a)
  (let* ((names (second a)))
    (when (or (not (listp names))
	      (not (> (length names) 1))
	      (not (symbolp (first names)))
	      (< (length a) 3))
      (defop-syntax-error names))
    (cond ((eql (third a) '! )
	   (when (< (length a) 5) (defop-syntax-error names))
	   (let* ((type (simplify-type (fourth a)))
		  (stype (nesl-read-type-list (cdar type)))
		  (dtype (nesl-read-type-exp (caar type)))
		  (name-args (nesl-read-exp (second a))))
	     (make-func :name (car name-args)
			:arguments (cdr name-args)
			:type (cons (list 'function nil dtype stype) 
				    (cdr type))
			:body (nesl-read-top (fifth a)) 
			:other (cddr (cdddr a)))))
	  (t
	   (let ((name-args (nesl-read-exp (second a))))
	     (make-func :name (car name-args)
			:arguments (cdr name-args)
			:body (nesl-read-top (third a))
			:other (cdddr a)))))))


(defun nesl-read-defrec (a) a)

(defun nesl-read-toplevel (exp if_primit)
  (cond ((listp exp)
	 (cond ((eql (car exp) 'defop)
		(if if_primit (nesl-read-prim-defop exp) (nesl-read-defop exp)))
	       ((eql (car exp) 'defrec)
		(nesl-read-defrec exp))
	       ((eql (car exp) 'deftypeclass)
		exp)
	       ((eql (car exp) 'set)
		`(assign ,(second exp) ,(nesl-read-exp (third exp))
			 ,(cdddr exp)))
	       ((member (car exp) 
			'(describe 
			  redefinep debug cnesl lisp 
			  exit help verbose load
			  set_print_length bugs configuration
			  use_machine list_machines
			  set_memory_size cm_finger progn))
		exp)
	       (t (nesl-read-exp exp))))
	(t exp)))


(defun nesl-loop3 (in-stream out-stream if_primit)
  (declare (ignore out-stream))
  (setq *line-num* 0)
  (let ((result nil)
	(func-def nil))
    (loop
     (catch 'nesl-error
       (when *eof* (setq *eof* nil) (return))
       (let ((readval (read in-stream nil :exit)))
	 (when (eql readval :exit) (return t))
	 (setq func-def  (nesl-read-toplevel readval if_primit))
	 (setq result (cons func-def result)))))
    (reverse result)))


;;; read from standard input	 
(defun lisp-parse (if_primit)
  (nesl-loop3 *standard-input* *standard-output* if_primit))


;;; read from a file
(defun lisp-file (filename if_primit)
  (with-open-file (in-stream filename :direction :input)
    (nesl-loop3 in-stream *standard-output* if_primit)))



