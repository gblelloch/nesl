;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

;; some things that need to be defined for "tokens.lisp" and "parse.lisp"

(defparameter *eof* nil)
(defparameter *default-list* nil)
(defparameter *current-fundef* nil)
(defparameter *current-typedef* nil)
(defparameter *current-filename* nil)
(defparameter *error* nil)
(defparameter *original-defs* nil)

(defun get-pscode (x) x)

(defun nesl-error (format-string &rest args)
  (if *current-fundef*
      (format t "~%Error in Function definition ~a.  " *current-fundef*)
    (if *current-typedef*
	(format t "~%Error in Datatype declaration ~a.  " *current-typedef*)
      (format t "~%Error at top level.  ")))
  (format t "~%")
  (apply 'format t format-string args)
  (format t "~%")
  (setq *error* t)
  (throw 'nesl-error :error)
  )

(defun nesl-runtime-error (format-string &rest args)
  (format t "~%Runtime Error")
  (format t "~%")
  (apply 'format t format-string args)
  (format t "~%~%")
  (throw 'nesl-runtime-error :error)
)


(defun nload-error ()
  (declare (special *load-error*))
  (setq *load-error* t)
  (throw 'nload-error :error))

(defun get-keyword (key lst default) 
  (if lst 
      (if (eql key (car lst)) 
          (second lst) 
        (get-keyword key (cddr lst) default))
    default))


(defun is-function-type? (type)
  (and (listp (car type))
       (eql (first (car type)) 'function)))


(defun intern-type (type)
  (cond ((symbolp type)
	 (if (vprefixp type)
	     (list 'vector (intern-type (vpostfix type)))
	   type))
	((listp type)
	 (cons (car type) (intern-type-list (cdr type))))
	(t
	 (nesl-error "The value ~a is an invalid type." type))))

(defun intern-type-list (type-list)
  (cond ((not type-list) nil)
	((eql (car type-list) 'v.)
	 (cons (list 'vector (intern-type (second type-list)))
	       (intern-type-list (cddr type-list))))
	((and (symbolp (car type-list)) (vprefixp (car type-list)))
	 (let ((foo (intern-type-list (cons (vpostfix (car type-list))
					    (cdr type-list)))))
	   (cons (list 'vector (car foo)) (cdr foo))))
	(t
	 (cons (intern-type (car type-list))
	       (intern-type-list (cdr type-list))))))

(defun simplify-types-internal (types)
  (if (listp types)
      (let* ((par-types (if (member '<- types) (list types) types)))
	(if (listp (car par-types))
	    (let ((long-types (intern-type-list (car par-types))))
	      (if  (eql (second long-types) '<-)
		  (cons (cons (car long-types) (cddr long-types)) 
			(cdr par-types))
		nil))
	  nil))
    nil))

(defun simplify-type (types)
  (or (simplify-types-internal types)
      (nesl-error "Bad typespec: ~a.~%The syntax of a typespec is:~%~%  ~
                   (typeexp <- typeexp*) |~%  ~
                   ((typeexp <- typeexp*) (Ident typeclass)*).~%"
		  types)))

(defun listify-type (type defs)
  (declare (special *current-typedef*))
  (cond	((listp type)
	 (cond ((eql (car type) 'function)
		(list 'function (second type) 
		      (listify-type (third type) defs)
		      (listify-type (fourth type) defs)))
	       ((eql (car type) 'pair)	
	(list 'pair 		   
		      (listify-type (second type) defs)
		      (listify-type (third type) defs)))
	       ((eql (car type) 'vector)
		(list 'vector (listify-type (second type) defs)))
	       (t
		(cons (car type) (listify-type-list (second type) defs)))))
	((or (is-prim-type? type) (not (get-type-def type defs)))
	 type)
	((eql type *current-typedef*)
	 (nesl-error "Recursive types are not permitted."))
	(t (list type))))

(defun listify-type-list (types defs)
  (if (and (listp types) (eql (car types) 'pair))
      (cons (listify-type (second types) defs)
	    (listify-type-list (third types) defs))
    (list (listify-type types defs))))

;; this recursively converts record types from the form 
;; (foo (pair x (pair y z))) to the form (foo x y z).
(defun listify-full-type (type defs)
  (if (null type) nil
    (cons (listify-type (car type) defs) (cdr type))))








