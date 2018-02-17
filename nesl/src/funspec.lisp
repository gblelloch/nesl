;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

(defstruct binding
  original-code
  compiled-code
  argcheck-code
  redefinable?
  (trace? 0)
  (time? nil)
  environment)

(defstruct (pscode (:print-function print-pscode))
  parallel
  serial
  serial-only?
  datatype?)

(defstruct (code (:print-function print-code))
  arguments
  type
  compiled
  cache
  name
  number)

(defstruct (func (:print-function print-func))
  name
  arguments
  type
  body
  other)

(defstruct typedef
  name
  type
  documentation
  redefine)

(defstruct definition-table
  bindings
  type-defs
  type-hash
  type-count
  prim-types
  constants)

;;;;;;;;;;;

(defun print-code (self stream depth)
  (declare (ignore depth))
  (format stream "CODE::~a~a" (code-name self) (code-number self)))

(defun print-pscode (self stream depth)
  (declare (ignore depth))
  (format stream "PSCODE::~a" (code-name (pscode-serial self))))

(defun print-func (self stream depth)
  (declare (ignore depth))
  (format stream "FUNC::~a" (func-name self)))

;;;;;;;;;;;;

(defun make-defs-table ()
  (make-definition-table 
   :type-hash (make-hash-table :test #'equal)
   :type-count 0))

;;;;;;;;;;;;;;;;;;;;;

(defun new-binding (func definitions)
  (let* ((old-definition (get-binding-definition (func-name func) definitions))
	 (number (if old-definition 
		     (1+ (code-number 
			  (pscode-serial 
			   (binding-compiled-code old-definition))))
		   0)))
    (make-binding :original-code func
		  :compiled-code 
		  (make-pscode :serial (make-code :number number)
			       :parallel (make-code :number number)))))

(defun add-binding-to-environment (binding definitions)
  (push (cons (func-name (binding-original-code binding)) binding)
	(definition-table-bindings definitions)))

(defun get-binding-definition (name definitions)
  (let* ((val (cdr (assoc name (definition-table-bindings definitions)))))
    (if (binding-p val) val nil)))

(defun get-pscode (name)
  (declare (special *definitions*))
  (let ((binding (get-binding-definition name *definitions*)))
    (when (not binding) 
      (nesl-error "Internal Error: PSCODE not found for ~a." name))
    (binding-compiled-code binding)))

(defun get-typenum (type definitions)
  (let* ((type-hash (definition-table-type-hash definitions))
	 (val (gethash type type-hash)))
    (if val val
      (setf (gethash type type-hash)
	    (incf (definition-table-type-count definitions))))))

(defun add-cached-code (codestruct argtypes code definitions)
  (let ((rcode (if (and (listp code) (listp (car code)))
		   (make-string-code codestruct argtypes code definitions)
		 code)))
    (push (cons (get-typenum argtypes definitions) rcode)
	  (code-cache codestruct))
    rcode))

(defun get-cached-code (code type definitions)
  (let ((typenum (get-typenum type definitions)))
    (cdr (assoc typenum (code-cache code)))))

(defun get-full-name (fundef type definitions)
  ;; Make sure that different functions with the same name get named
  ;; differently in VCODE.
  (let* ((name (code-name fundef))
	 (number (code-number fundef)))
    (intern
     (if (= number 0)
	 (format nil "~A_~d" name (get-typenum type definitions))
       (format nil "~A~d_~d" name number (get-typenum type definitions))))))

;;;;;;;;;;;;;;;;;;;;;

(defun add-type-def (name type definitions)
  (let* ((typestruct (make-typedef :name name :type type)))
    (push (cons name typestruct)
	  (definition-table-type-defs definitions))))

(defun get-typedef (name definitions)
  (let ((val (cdr (assoc name (definition-table-type-defs definitions)))))
    (if (typedef-p val) val nil)))

;;;;;;;;;;;;;;;;

(defun add-prim-type (typename definitions)
  (push (cons typename nil) 
	(definition-table-prim-types definitions)))

(defun add-type-class (typename subtypes definitions)
  (labels ((get-subtypes (subtype)
	     (let* ((typelist (definition-table-prim-types definitions))
		    (subsubtypes (cdr (assoc subtype typelist))))
	       (if subsubtypes 
		   (mapcan #'get-subtypes subsubtypes)
		 (list subtype)))))
    (push (cons typename subtypes)
	  (definition-table-prim-types definitions))))

(defun subtypes (type definitions)
  (assoc type (definition-table-prim-types definitions)))

(defun primitive-type? (type definitions)
  (let ((foo (assoc type (definition-table-prim-types definitions))))
    (and foo (not (cdr foo)))))

;;;;;;;;;;;;;;;

(defun add-nondefinable-constant (name definitions)
  (push name (definition-table-constants definitions)))

(defun nondefinable-constant? (name definitions)
  (member name (definition-table-constants definitions)))
