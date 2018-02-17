;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)


(defun strip-pattern-exp (exp environment)
  (if (atom exp) exp
    (if (eql (first exp) 'pair)
	(list 'pair 
	      (strip-pattern-exp (second exp) environment)
	      (strip-pattern-exp (third exp) environment))
      (if (eql (first exp) 'vector)
	  (list 'vector (strip-pattern-exp (second exp) environment))
	(strip-pattern-exp (second exp) environment)))))

(defun strip-let-or-over (exp environment)
  (let ((pattern-exp (strip-pattern-exp (first (second exp)) environment))
	(binding-exp (strip-exp (second (second exp)) environment))
	(body-exp    (strip-exp (third exp) environment)))
    (list (first exp) (list pattern-exp binding-exp) body-exp)))

(defun strip-if (exp environment)
  (list `if 
	(strip-exp (second exp) environment)
	(strip-exp (third exp) environment)
	(strip-exp (fourth exp) environment)))

(defun strip-pair (exp environment)
  (list (first exp)
	(strip-exp (second exp) environment)
	(strip-exp (third exp) environment)))

(defun strip-func (exp environment)
  (list (strip-exp (first exp) environment)
	(strip-exp (second exp) environment)))

(defun strip-exp (exp environment)
  (cond ((nesl-constant-p exp) exp)
	((symbolp exp) exp)
	((pscode-p exp) exp)
	((listp exp)
	 (cond ((eql (car exp) 'let)
		(strip-let-or-over exp environment))
	       ((eql (car exp) 'over)
		(strip-let-or-over exp environment))
	       ((eql (car exp) 'if)
		(strip-if exp environment))
	       ((eql (car exp) 'pair)
		(strip-pair exp environment))
	       ((eql (car exp) 'closure)
		(strip-pair exp environment))
	       (t (strip-func exp environment))))
	(t (nesl-error "Internal error: Invalid expression, ~s." exp))))

(defun strip-type-exp (typeexp tstack defs)
  (cond ((and (atom typeexp) (assoc typeexp tstack))
	 (cdr (assoc typeexp tstack)))
	((symbolp typeexp) typeexp)
	((listp typeexp)
	 (cond ((eql (car typeexp) 'pair)
		`(pair ,(strip-type-exp (second typeexp) tstack defs)
			     ,(strip-type-exp (third typeexp) tstack defs)))
	       ((eql (car typeexp) 'function)
		`(function ,(second typeexp)
			   ,(strip-type-exp (third typeexp) tstack defs)
			   ,(strip-type-exp (fourth typeexp) tstack defs)))
	       ((eql (car typeexp) 'vector)
		`(vector ,(strip-type-exp (second typeexp) tstack defs)))
	       (t
		(let* ((type-dec (typedef-type
				  (get-typedef (first typeexp) defs)))
		       (nstack
			(mapcar #'(lambda (subtype var) 
				    (cons (car var) 
					  (strip-type-exp subtype tstack 
							  defs)))
				(cdr typeexp)
				(cdr type-dec))))
		  (strip-type-exp (second (car type-dec)) nstack defs)))))
	(t (nesl-error "Internal error: invalid type expression."))))

(defun strip-type (type defs) 
  (let ((type-exp (car type))
	(context (cdr type)))
    (cons (strip-type-exp type-exp nil defs) context)))

;;;;;;;;;;
;;; PARALLEL CHECK (checks to see if a function is callable in parallel
;;;;;;;;;;

(defun parallel-check-let (exp p?)
  (let ((binding-exp (parallel-check-exp (second (second exp)) p?))
	(body-exp    (parallel-check-exp (third exp) p?)))
    (and binding-exp body-exp)))

(defun parallel-check-over (exp p?)
  (let ((binding-exp (parallel-check-exp (second (second exp)) p?))
	(body-exp    (parallel-check-exp (third exp) t)))
    (declare (ignore body-exp))
    binding-exp))

(defun parallel-check-if (exp p?)
  (and (parallel-check-exp (second exp) p?)
       (parallel-check-exp (third exp) p?)
       (parallel-check-exp (fourth exp) p?)))

(defun parallel-check-pair (exp p?)
  (and (parallel-check-exp (second exp) p?)
       (parallel-check-exp (third exp) p?)))

(defun parallel-check-func (exp p?)
  (and (parallel-check-exp (first exp) p?)
       (parallel-check-exp (second exp) p?)))

(defun parallel-check-exp (exp p?)
  (cond ((null exp) t)
	((nesl-constant-p exp) t)
	((symbolp exp) t)
	((pscode-p exp) 
	 (when (and p? (pscode-serial-only? exp))
	   (nesl-error "Cannot call ~a in parallel." 
		       (code-name (pscode-serial exp))))
	 (not (pscode-serial-only? exp)))
	((listp exp)
	 (cond ((eql (car exp) 'let)
		(parallel-check-let exp p?))
	       ((eql (car exp) 'over)
		(parallel-check-over exp p?))
	       ((eql (car exp) 'if)
		(parallel-check-if exp p?))
	       ((eql (car exp) 'pair)
		(parallel-check-pair exp p?))
	       (t (parallel-check-func exp p?))))
	(t (nesl-error "Internal error: Invalid expression, ~s." exp))))

(defun parallel-check-op (exp)
  (if (listp exp)
      (cond ((eql (car exp) 'poly-typecase)
	     (and (parallel-check-exp (third exp) nil)
		  (parallel-check-exp (fourth exp) nil)
		  (parallel-check-exp (fifth exp) nil)))
	    ((eql (car exp) 'base-typecase)
	     (and (parallel-check-exp (second (third exp)) nil)
		  (parallel-check-exp (second (fourth exp)) nil)
		  (parallel-check-exp (second (fifth exp)) nil)
		  (parallel-check-exp (second (sixth exp)) nil)
		  (parallel-check-exp (second (seventh exp)) nil)))
	    (t (parallel-check-exp exp nil)))
    (parallel-check-exp exp nil)))
