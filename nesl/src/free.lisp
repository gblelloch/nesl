;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

;;; The routines in this file take an NESL expression and mark all the
;;; final references to variables with LAST.   This, in turn, is
;;; used by the compiler to deallocate the variable whenever it
;;; sees a reference to last.  
;;; As well as taking the expression, the routines in the file take
;;; a list of continuation variables (variable references that appear
;;; in the continuation).  Any variable in this list will not be marked
;;; with LAST.
;;; Each of the routines in the file returns two values in which the first 
;;; item is a list of free variables that appear in the body, and the
;;; second is a new body with the appropriate transformations made.
;;;
;;; An example:
;;;  (free-exp 
;;;    '(let (x (+ x y))
;;;       (let (z (pack w q))
;;;         (- (+ (* x 7) (+_reduce z)) y)))
;;;    '(w))
;;;
;;; will return:
;;;
;;;     (Q W Y X) 
;;;
;;;     (LET (X (+ (LAST X) Y)) 
;;;       (LET (Z (PACK W (LAST Q)))
;;;         (- (+ (* (LAST X) 7) (+_REDUCE (LAST Z))) (LAST Y))))
;;;
;;; The FREE-IF function also adds some information to if statements.
;;; In particular it adds a list of the free variables that are 
;;; freed (marked with LAST) in both the IF part and the ELSE part.

(defun free-symbol (exp cont-vars)
  (values (list exp)
	  (if (find exp cont-vars) exp (list 'last exp))))

(defun free-let (exp cont-vars)
  (let* ((pattern-exp (first (second exp)))
	 (binding-exp (second (second exp)))
	 (body-exp (third exp))
	 (pattern-vars (bindvars-exp pattern-exp)))
    (multiple-value-bind (body-vars body-code)
      (free-exp body-exp (set-difference cont-vars pattern-vars))
      (let ((free-body-vars (set-difference body-vars pattern-vars))
	    (unused-vars (set-difference pattern-vars body-vars)))
	(multiple-value-bind (bind-vars bind-code)
	  (free-exp binding-exp (union free-body-vars cont-vars))
	  (values (union free-body-vars bind-vars)
		  `(let (,pattern-exp ,bind-code ,unused-vars) 
		     ,body-code)))))))

(defun free-if (exp cont-vars)
  (multiple-value-bind (else-vars else-code) 
    (free-exp (fourth exp) cont-vars)
    (multiple-value-bind (then-vars then-code) 
      (if (eql (third exp) 'then-body-hack) 
	  (values nil 'then-body-hack)
	(free-exp (third exp) cont-vars))
      (let ((then-else-vars (union else-vars then-vars)))
	(multiple-value-bind (cond-vars cond-code)
	  (free-exp (second exp) (union cont-vars then-else-vars))
	  (values (union then-else-vars cond-vars)
		  (list 'if cond-code then-code else-code
			(set-difference then-vars cont-vars)
			(set-difference else-vars cont-vars))))))))

(defun free-pair (exp cont-vars)
  (multiple-value-bind (second-vars second-code)
    (free-exp (third exp) cont-vars)
    (multiple-value-bind (first-vars first-code)
      (free-exp (second exp) (union cont-vars second-vars))
      (values (union second-vars first-vars) 
	      (list (first exp) first-code second-code)))))

(defun free-parallel (exp cont-vars)
  (multiple-value-bind (arg-vars arg-code)
    (free-exp (cdr exp) cont-vars)
    (values arg-vars (cons (car exp) arg-code))))

(defun free-func (exp cont-vars)
  (multiple-value-bind (arg-vars arg-code)
    (free-exp (second exp) cont-vars)
    (multiple-value-bind (func-vars func-code)
      (free-exp (first exp) (union cont-vars arg-vars))
      (values (union arg-vars func-vars)
	      (list func-code arg-code)))))

(defun free-exp (exp cont-vars)
  (cond ((nesl-constant-p exp) 
	 (values nil exp))
	((pscode-p exp) 
	 (values nil exp))
	((symbolp exp)
	 (free-symbol exp cont-vars))
	((listp exp)
	 (cond ((eql (car exp) 'let)
		(free-let exp cont-vars))
	       ((eql (car exp) 'if)
		(free-if exp cont-vars))
	       ((eql (car exp) 'pair)
		(free-pair exp cont-vars))
	       ((eql (car exp) 'closure)
		(free-pair exp cont-vars))
	       ((eql (car exp) 'parallel)
		(free-parallel exp cont-vars))
	       (t (free-func exp cont-vars))))
	(t (error "Invalid expression, ~s." exp))))

(defun free-body (exp)
  (multiple-value-bind (exp-vars exp-code)
    (free-exp exp nil)
    (declare (ignore exp-vars))
    exp-code))
