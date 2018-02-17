;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)


(defun listify-pairs (args)
  (cond 
   ((atom args) (list args))
   (t (if (eql (first args) 'pair) (append (listify-pairs (second args))
					   (listify-pairs (third args)))
	(listify-pairs (second args))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; the following ext functions find all the free variables in a body
;;;;;;;;;;;;;;;;;;;;;;;;

(defun ext-let-or-over (exp env)
  (nunion (ext-exp (second (second exp)) env)
	 (ext-exp (third exp)
		  (nunion (listify-pairs (first (second exp)))
			 env))))

(defun ext-func (exp env)
  (let ((func-ext (ext-exp (first exp) env))
	(arg-ext (ext-exp (second exp) env)))
    (nunion func-ext arg-ext)))

(defun ext-pair (exp env)
  (union (ext-exp (second exp) env) (ext-exp (third exp) env)))

(defun ext-symbol (exp env)
  (if (not (find exp env)) (list exp) nil))

(defun ext-if (exp env)
  (union (ext-exp (second exp) env)
	 (union
	  (ext-exp (third exp) env) 
	  (ext-exp (fourth exp) env))))

(defun ext-exp (exp env)
  (cond ((nesl-constantp exp) nil)
	((symbolp exp)
	 (ext-symbol exp env))
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

