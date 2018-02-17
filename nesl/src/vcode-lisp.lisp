;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

;;; Interface code between vcode and lisp.
;;; (First pass written by Timothy Freeman)

(in-package :nesl-lisp) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFINING F TO BE NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun booleanp (x) (member x '(t f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFINITION OF THE MACRO CHARACTERS #v AND #u
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter vcode-vector-letter #\u
  "The letter used for low-level vcode-vectors.")

(defparameter vcode-sequence-letter #\v
  "The letter used for higher-level nestable vcode-vectors.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFINITION OF STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We use structures vcode-vector, vcode-tuple, and vcode-record
;;; instead of simple lists so we can figure out a unique vcode
;;; type for each lisp data structure.  
(defstruct (vcode-vector (:print-function print-vcode-vector))
  type
  data)

(defstruct (nested-sequence (:print-function print-nested-sequence)) data type)

(defstruct (nesl-struct (:print-function print-nesl-struct)) data type)

(defun print-vcode-vector (self stream depth)
  (declare (ignore depth))
  (if (and (= (length (vcode-vector-data self)) 1)
	   (not (eql (vcode-vector-type self) 'vector)))
      (format stream "~s" (elt (vcode-vector-data self) 0))
      (format stream "#~a.~s~:s"
	      vcode-vector-letter
	      (vcode-vector-type self)
	      (coerce (vcode-vector-data self) 'list))))

(defun print-nested-sequence (self stream depth)
  (declare (ignore depth) (special *max-print-length* *cnesl-syntax*))
  (let ((data (nested-sequence-data self))
	(type (nested-sequence-type self)))
    (cond ((eql type 'char)
	   (format stream "~s" (coerce data 'string)))
	  ((= (length data) 0)
	   (if *cnesl-syntax*
	       (format stream "[]" vcode-sequence-letter)
	     (format stream "#~a()" vcode-sequence-letter)))
	  ((<= (length data) *max-print-length*)
	   (if *cnesl-syntax*
	       (format stream "[~s~{,~s~}]" (car data) (cdr data))
	     (format stream "#~a~s" vcode-sequence-letter data)))
	  (t 
	   (let ((data (subseq data 0 *max-print-length*)))
	     (if *cnesl-syntax*
		 (format stream "[~s~{,~s~},...]" (car data) (cdr data))
	       (format stream "#~a(~{~s ~}...)" vcode-sequence-letter)))))))

(defun print-nesl-struct (self stream depth)
  (declare (ignore depth) (special *cnesl-syntax*))
  (let ((data (nesl-struct-data self))
	(type (nesl-struct-type self)))
    (cond ((not *cnesl-syntax*)
	   (format stream "(~s~{ ~s~})" type data))
	  ((eql type 'pair)
	   (format stream "(~s,~s)" (first data) (second data)))
	  (t 
	   (format stream "~s(~s~{,~s~})" type (car data) (cdr data))))))

(defun nesl-floatp (val)
  (or (floatp val) (eql val 'infinity)))

(defparameter *typecheck-map*
  '((int . integerp) (bool . booleanp) (float . nesl-floatp) 
    (char . characterp) (segdes . integerp)
    (stream . integerp)))

(defun make-vcode-vector-safely (type data)
  (let* ((itype (or type 
		   (if (= (length data) 0)
		       (nesl-error "Can't determine the type of empty vector.")
		     (typeof-scalar (car data)))))
	(cfunc (cdr (assoc itype *typecheck-map*))))
    (when (not cfunc)
      (nesl-error "~a is an invalid type for a constant vector." itype))
    (when (not (every cfunc data))
      (nesl-error "Inhomogeneous values in a constant vector."))
    (make-vcode-vector :type itype :data data)))

(defun make-vcode-vector-semi-safely (type data)
  (let ((cfunc (cdr (assoc type *typecheck-map*))))
    (when (not cfunc)
      (nesl-error "~a is an invalid type for a constant vector." type))
    (when (not (or (null data) (funcall cfunc (car data))))
      (nesl-error "Bad value in a result vector."))
    (make-vcode-vector :type type :data data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STUFF FOR READING VECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sequence-reader-top-level* t)

(defun read-type (head stream)
  (cond ((eql head 'v.)
	 (list `vector (intern-type (read stream t nil t))))
	((and (symbolp head) (vprefixp head))
	 (list 'vector (read-type (vpostfix head) stream)))
	(t (intern-type head))))

(defun read-vcode-vector (stream subchar arg)
  (declare (ignore subchar arg))
  (if *read-suppress* nil
    (let ((type (cond ((char= (peek-char nil stream) #\.)
		       (read-char stream t nil t)
		       (read stream t nil t))
		      (t nil))))
      (make-vcode-vector-safely type (read stream t nil t)))))

(defun read-vcode-sequence (stream subchar arg)
  (declare (ignore subchar arg))
  (if *read-suppress* nil
    (if *sequence-reader-top-level*
	(let ((*sequence-reader-top-level* nil))
	  (declare (special *sequence-reader-top-level*))
	  (flatten-nesl-type (sequence-reader stream)))
      (sequence-reader stream))))

(set-dispatch-macro-character #\# vcode-vector-letter #'read-vcode-vector)

(set-dispatch-macro-character #\# vcode-sequence-letter #'read-vcode-sequence)

(defun sequence-reader (stream)
  (declare (special *definitions*))
  (cond ((char= (peek-char nil stream) #\.)
	 (read-char stream t nil t)
	 (make-nested-sequence 
	  :type (expand-type (read-type (read stream t nil t) stream)
			     *definitions*)
	  :data nil))
	(t (make-nested-sequence :type nil :data (read stream t nil t)))))

(defun typeof-scalar (prim-val)
  (cond ((booleanp prim-val) 'bool)
	((integerp prim-val) 'int)
	((floatp prim-val) 'float)
	((characterp prim-val) 'char)
	(t (nesl-error "Invalid type ~a in a NESL constant." 
		       (type-of prim-val)))))

(defun typeof-vector (prim-vect)
  (cond ((nested-sequence-p prim-vect)
	 `(vector 
	   (pair
	    segdes
	    ,(or (nested-sequence-type prim-vect) 
	      (if (= (length (nested-sequence-data prim-vect)) 0)
		  (nesl-error "Can't figure out the type of an empty vector.")
		(typeof-vector (car (nested-sequence-data prim-vect))))))))
	((stringp prim-vect) '(vector (pair segdes char)))
	((listp prim-vect) 
	 (cons (car prim-vect) 
	       (mapcar 'typeof-vector (cdr prim-vect))))
	(t (typeof-scalar prim-vect))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STUFF FOR FLATTENING A VECTOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flatten-slot (list)
  (if (not list) nil
    (append (car list) (flatten-slot (cdr list)))))

(defun flatten-list (prim-vect type)
  (if (not type)
      (if (not (every #'null prim-vect))
	  (nesl-error "Inhomogeneous structure values in a constant vector.")
	nil)
    (cons (flatten-exp (mapcar #'car prim-vect) (car type))
	  (flatten-list (mapcar #'cdr prim-vect) (cdr type)))))

(defun flatten-exp (prim-vect type)
  (cond ((atom type)
	 (make-vcode-vector-safely type prim-vect))
	((eql (car type) 'vector)
	 (let ((subvects 
		(mapcar 
		 #'(lambda (a)
		     (cond ((stringp a) (coerce a 'list))
			   ((nested-sequence-p a)
			    (nested-sequence-data a))
			   (t (nesl-error 
			       "Inhomogeneous types in a constant vector."))))
		 prim-vect)))
	   (list (get-pscode 'vector)
		 (list 'pair
		       (make-vcode-vector-safely 
			'segdes (mapcar #'length subvects))
		       (flatten-exp (flatten-slot subvects) 
				    (third (second type)))))))
	((listp type)
	 (cons (car type)
	       (flatten-list (mapcar #'cdr prim-vect) (cdr type))))))

(defun flatten-nesl-type (val)
  (flatten-exp (list val) (typeof-vector val)))

;;; This returns the type if the object is a nesl constant
;;; Otherwise it returns nil
(defun nesl-constant-p (val)
   (cond ((vcode-vector-p val) (vcode-vector-type val))
	 ((booleanp val) 'bool)
	 ((integerp val) 'int)
	 ((floatp val) 'float)
	 ((characterp val) 'char)
	 ((stringp val) '(vector char))))

;;; This coerces a NESL constant into a vcode-vector
;;; It will fail if passed anything other than a nesl constant
(defun coerce-nesl-constant (val)
  (if (vcode-vector-p val) val
    (flatten-nesl-type val)))

(defun make-empty-vector (type defs)
  (flatten-exp (list (make-nested-sequence :data nil))
	       (expand-type (list 'vector type) defs)))

(defun make-empty-constant (type defs)
  (flatten-exp nil (expand-type type defs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRINTING NESL VECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *print-vcode-sequences* t
  "Whether to print things of type (vector foo) as #v(...).")

(defun partition-vector (lengths values)
  (if lengths
      (cons (subseq values 0 (car lengths))
	    (partition-vector (cdr lengths) (nthcdr (car lengths)  values)))
    (if values (nesl-error "Lengths don't match in a Nesl structure.")
      nil)))

(defun zipcons (a b)
  (if (= (length a) 0)
      (if (= (length b) 0)
	  nil
	(nesl-error "Lengths don't match in a Nesl structure."))
    (if (= (length b) 0)
	(nesl-error "Lengths don't match in a Nesl structure.")
      (cons (cons (car a) (car b)) (zipcons (cdr a) (cdr b))))))

(defun nest-constant-list (list length)
  (multiple-value-bind (car-result car-type) 
    (nest-constant-exp (car list) length)
    (if (= (length list) 1)
	(values (mapcar #'list car-result) (list car-type))
      (multiple-value-bind (cdr-result cdr-type) 
	(nest-constant-list (cdr list) length)
        (values (zipcons car-result cdr-result) 
		(cons car-type cdr-type))))))

(defun nest-constant-exp (data length)
  (cond ((vcode-vector-p data) 
	 (values (vcode-vector-data data) (vcode-vector-type data)))
	((and (listp data) 
	      (or (eql (car data) 'vector)
		  (and (pscode-p (car data))
		       (eql (code-name (pscode-serial (car data)))
			    'vector))))
	 (let* ((segdes (second (second data)))
		(subdata (third (second data))))
	   (multiple-value-bind (subvals type) 
	     (nest-constant-exp subdata 
				(reduce #'+ (vcode-vector-data segdes)))
	     (values 
	      (mapcar #'(lambda (a) 
			  (make-nested-sequence :data a :type type))
		      (partition-vector (vcode-vector-data segdes) 
					subvals))
	      `(vector (pair segdes ,type))))))
	((pscode-p data)
	 (values (make-sequence 'list length :initial-element "fn")
		 (code-type (pscode-serial data))))
	((and (listp data) (eql (car data) 'closure))
	 (values (make-sequence 'list length :initial-element "fn")
		 (code-type (pscode-serial (second data)))))
	((and (listp data) (> (length data) 1))
	 (multiple-value-bind (result type) 
	   (nest-constant-list (cdr data) length)
	   (values (mapcar #'(lambda (a) 
			       (make-nesl-struct :type (car data) :data a))
			   result)
		   (cons (car data) type))))
	((and (listp data) (= (length data) 1))
	 (nesl-error "NESL currently cannot print empty records."))
	(t
	 (nesl-error "Bad constant in nest-constant-exp."))))

(defun nest-constant (data)
  (if (vcode-vector-p data)
      data
    (car (nest-constant-exp data 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GROUPING DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun group-data-exp (type data)
  (cond ((atom type) 
	 (cons (make-vcode-vector-semi-safely
		type
		(if (eql type 'char)
		    (mapcar #'code-char (car data))
		  (car data)))
	       (cdr data)))
	((eql (car type) 'function)
	 (if (listp (second type))
	     (let ((grouped-rest (group-data-exp (third (second type)) data)))
	       (cons (list 'closure (second (second type)) (car grouped-rest))
		     (cdr grouped-rest)))
	   (cons (binding-compiled-code (second type)) data)))
	(t
	 (let ((grouped-list (group-data-list (cdr type) data)))
	   (cons (cons (car type) (car grouped-list))
		 (cdr grouped-list))))))

(defun group-data-list (type data)
  (if type
      (let* ((head (group-data-exp (car type) data))
	     (tail (group-data-list (cdr type) (cdr head))))
	(cons (cons (car head) (car tail))
	      (cdr tail)))
    (cons nil data)))

(defun group-data (data type definitions)
  (car (group-data-exp (expand-type type definitions) data)))
