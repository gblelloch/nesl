;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

(defun type-< (type1 type2 definitions)
  (if (eql type2 'any) 
      t
    (member type1 (subtypes type2 definitions))))

(defun variable-type? (type definitions)
  (or (eql type 'any)
      (> (length (subtypes type definitions)) 1)))

(defun variable-subtypestype? (type definitions)
  (or (eql type 'any)
      (subtypes type definitions)))

(defun is-function-type? (type)
  (and (listp (car type))
       (eql (first (car type)) 'function)))

(defparameter *allow-type-vars* nil)

(defun parse-type-exp (type tstack sum-func prim-func list-func
			    func-func zero definitions)
  (cond ((null type) zero)
	((and (atom type) (primitive-type? type definitions))
	 (funcall prim-func type))
	((and (atom type) (assoc type tstack))
	 (cdr (assoc type tstack)))
	;;((and (atom type) (get-typedef type definitions))
	;;(let ((def-type (typedef-type (get-typedef type definitions))))
	;;(if (/= (length (caar def-type)) 1)
	;;(nesl-error "Bad type expression ~a"
	;;(pprint-nesl-string type 2))
	;;(funcall list-func type
	;;(parse-type-list (cdr (car def-type)) nil sum-func 
	;;prim-func list-func func-func
	;;zero definitions)))))
	((listp type)
	 (if (eql (car type) 'function) 
	     (funcall func-func type)
	   (let ((typedef (get-typedef (car type) definitions)))
	     (if (not typedef)
		 (nesl-error "Bad type expression ~a"
			     (pprint-nesl-string type 2))
	       (let* ((type-dec (typedef-type typedef))
		      (nstack
		       (mapcar #'(lambda (subtype var) 
				   (cons (car var) 
					 (parse-type-exp 
					  subtype tstack sum-func prim-func
					  list-func func-func zero 
					  definitions)))
			       (cdr type)
			       (cdr type-dec))))
		 (funcall 
		  list-func 
		  (car type)
		  (parse-type-list (cdr (car type-dec)) nstack sum-func 
				   prim-func list-func func-func
				   zero definitions)))))))
	(t (if *allow-type-vars*
	       (funcall prim-func type)
	     (nesl-error "No type definition for ~a." 
			 (pprint-nesl-short-string type))))))

(defun parse-type-list (type tstack sum-func prim-func list-func func-func
			     zero defs)
  (if type
      (funcall sum-func 
	       (parse-type-exp (car type) tstack sum-func 
			       prim-func list-func func-func zero defs)
	       (parse-type-list (cdr type) tstack sum-func 
				prim-func list-func func-func zero defs))
    zero))

(defun flatten-type (type definitions)
  (parse-type-exp type nil #'append 
		  #'(lambda (a) (list a))
		  #'(lambda (a b) (declare (ignore a)) b) 
		  #'(lambda (a) (if (listp (second a))
				    (flatten-type (third (second a)) 
						  definitions)
				  nil))
		  nil definitions))

(defun expand-type (type definitions)
  (parse-type-exp type nil #'cons #'(lambda (a) a) 
		  #'cons #'(lambda (a) a) nil definitions))

(defun expand-type-allow (type definitions)
  (let ((*allow-type-vars* t))
    (declare (special *allow-type-vars*))
    (parse-type-exp type nil #'cons #'(lambda (a) a) 
		    #'cons #'(lambda (a) a) nil definitions)))

(defun andfunc (a b) (or a b))

(defun check-type-list (full-type definitions)
  (flet ((check-var-type (type) 
 	   (if (and (listp type) (= (length type) 2))
	       (if (variable-type? (second type) definitions)
		   (cons (first type) t)
		 (nesl-error "The type ~a is not a valid type-class."
			      (second type)))
	     (nesl-error "The typebinding ~a should be of the form: ~
                          (type-var type-class)."
			  type))))
	(parse-type-list 
	 (car full-type) (mapcar #'check-var-type (cdr full-type))
	 #'andfunc #'(lambda (a) (declare (ignore a)) t)
	 #'(lambda (a b) (declare (ignore a)) b)
	 #'(lambda (a) (declare (ignore a)) t) 
	 nil definitions)))

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

;;; ******************
;;; THe following is a special case of code given above.
;;; It was inserted for efficiency reasons
;;; ****************

(defun l-from-type (type definitions)
  (cond ((null type) 0)
	((and (atom type) (primitive-type? type definitions)) 1)
	((listp type)
	 (cond ((or (eql (car type) 'pair)
		    (eql (car type) 'closure))
		(+ (l-from-type (second type) definitions)
		   (l-from-type (third type) definitions)))
	       ((eql (car type) 'vector)
		(+ 1 (l-from-type (second type) definitions)))
	       ((eql (car type) 'function)
		(if (listp (second type))
		    (l-from-type (third (second type)) definitions)
		  0))
	       (t (nesl-error "Bad type ~a." type))))
	(t (nesl-error "No base-type definition for ~a." type))))

(defun match-types (pattern exp type defs)
  (cond ((symbolp pattern) (list (list pattern exp type)))
	((and (listp pattern) (eql (car pattern) 'pair))
	 (if (and (listp exp) (eql (car exp) 'pair))
	     (append 
	      (match-types (second pattern) (second exp) (second type) defs)
	      (match-types (third pattern) (third exp) (third type) defs))
	   nil))
	((listp pattern)
	 (if (and (listp exp) (eql (car pattern) (car exp)))
	     (match-types (second pattern) (second exp)
			  (second (expand-type type defs)) defs)
	   nil))
	(t (nesl-error "The value ~a is not a valid variable name." 
		       (pprint-nesl-short-string pattern)))))

(defun listify-type (type defs)
  (declare (special *current-typedef*))
  (if (listp type)
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
	     (cons (car type) (listify-type-list (second type) defs))))
    (if (or (primitive-type? type defs)
	    (not (get-typedef type defs)))
	type
      (if (eql type *current-typedef*)
	  ;; A hack
	  (progn 
	    (clear-nesl-input)
	    (nesl-error "Recursive types are not permitted."))
	(list type)))))

(defun listify-type-list (types defs)
  (if (and (listp types) (eql (car types) 'pair))
      (cons (listify-type (second types) defs)
	    (listify-type-list (third types) defs))
    (list (listify-type types defs))))
