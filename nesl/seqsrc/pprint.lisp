;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

(defparameter *binops*
  '((or 2) (nor 2) (xor 2)
    (and 3) (nand 3)
    (== 4) (= 4 " == ") (/= 4) (< 4) (> 4) 
    (<= 4) (>= 4)
    (+ 5) (- 5) (++ 5) (<- 5)
    (* 6) (/ 6) (-> 6)
    (pad-string 6 " || ")  (|\|\|| 6 " || ")
    (power- 7 " ^ ") (^ 7)))

(defparameter *unaryops*
  '((vec-len |#|) (|#|) 
    (negate -) 
    (mk-string @) (@)))

(defun get-print-name (function)
  (or (third (assoc function *binops*))
      (second (assoc function *unaryops*))
      function))

;;;;;;;;;;;;;;;;;;;
;;;  PRETTY PRINT UTILITIES
;;;;;;;;;;;;;;;;;;;

(defun inc-lines (list inc)
  (if (null list) nil
    (cons (cons (+ inc (car (car list))) (cdr (car list)))
	  (inc-lines (cdr list) inc))))

(defun new-line (str)
  (list (cons 0 str)))

(defun append-lines-2 (lines1 lines2 &optional (max-length 120))
  (if (= (length lines1) 1)
      (let* ((line1 (car lines1))
	    (length (+ (car line1) (length (cdr line1)))))
	(if (> (+ length (length (cdr (car lines2)))) max-length)
	    (cons line1 (inc-lines lines2 (car line1)))
	  (cons (cons (car line1)
		      (format nil "~a~a" (cdr line1) (cdr (car lines2))))
		(inc-lines (cdr lines2) length))))
    (cons (car lines1) (append-lines-2 (cdr lines1) lines2 max-length))))

(defun append-lines-loop (lines-list)
  (if (= (length lines-list) 1) 
      (car lines-list)
    (append-lines-2 (car lines-list) (append-lines-loop (cdr lines-list)))))

(defun append-lines (lines1 &rest lines-list)
  (append-lines-loop (cons lines1 lines-list)))

(defun cond-append (str lines indent)
  (if (= (length lines) 1)
      (append-lines (new-line str) lines)
    (append (new-line str) (inc-lines lines indent))))

(defun cond-wrap-paren (flag body)
  (if flag
      (append-lines (new-line "(") body (new-line ")"))
    body))

;;;;;;;;;;;;;;;;;;;
;;;  PRETTY PRINT ROUTINES
;;;;;;;;;;;;;;;;;;;

(defun cnesl-with-binding (a)
  (let ((pattern-lines (cnesl-exp (first a) 1))
	(exp-lines (cnesl-exp (second a))))
    (append-lines pattern-lines (new-line " = ") exp-lines)))

(defun cnesl-with-bindings (a)
  (let ((bind1 (cnesl-with-binding (car a))))
    (if (= (length a) 1) bind1
      (append (append-lines bind1 (new-line "; "))
	      (cnesl-with-bindings (cdr a))))))

(defun unzip-let (exp)
  (if (and (listp (third exp)) (eql (first (third exp)) 'let))
      (multiple-value-bind (subbinds subbody)
	(unzip-let (third exp))
	(values (cons (second exp) subbinds) subbody))
    (values (list (second exp)) (third exp))))

(defun cnesl-let (exp prec)
  (multiple-value-bind (bindings body)
    (unzip-let exp)
    (cond-wrap-paren 
     (> prec 0)
     (append
      (cond-append "let " (cnesl-with-bindings bindings) 2)
      (cond-append "in "  (cnesl-exp body) 2)))))

(defun cnesl-over-binding (a)
  (if (and (symbolp (first a)) (eql (first a) (second a)))
      (cnesl-exp (first a))
    (let ((pattern-lines (cnesl-exp (first a) 1))
	  (exp-lines (cnesl-exp (second a))))
      (append-lines pattern-lines (new-line " in ") exp-lines))))

(defun cnesl-over-bindings (previous-binds a)
  (if (null a) previous-binds
    (cnesl-over-bindings
     (append-lines-2 (append-lines previous-binds (new-line "; "))
		     (cnesl-over-binding (car a))
		     50)
     (cdr a))))

(defun unzip-over (pattern exp)
  (if (and (listp exp) (eql-pscode (car exp) 'zip-over))
      (cons (list (second pattern) (second (second exp)))
	    (unzip-over (third pattern) (third (second exp))))
    (list (list pattern exp))))

(defun cnesl-over (binding exp &optional sieve)
  (let* ((bindings (unzip-over (first binding) (second binding)))
	 (binding-lines 
	  (cnesl-over-bindings (cnesl-over-binding (car bindings))
			       (cdr bindings)))
	 (head 
	  (if (eql (first (car bindings)) exp)
	      binding-lines
	    (append-lines-2 (append-lines (cnesl-exp exp 1) (new-line ": "))
			    binding-lines 50)))
	 (body
	  (if sieve 
	      (append-lines-2 (append-lines head (new-line " | "))
			      (cnesl-exp sieve) 50)
	    head)))
    (append-lines (new-line "{") body (new-line "}"))))

(defun cnesl-if (cond thenp elsep prec)
  (let ((if-lines (cnesl-exp cond 10))
	(then-lines (cnesl-exp thenp))
	(else-lines (cnesl-exp elsep)))
    (cond-wrap-paren 
     (> prec 0)
     (append 
      (cond-append "if " if-lines 2)
      (cond-append "then " then-lines 2)
      (cond-append "else " else-lines 2)))))

(defun cnesl-infix (op p1 p2 currentprec lprec rprec)
  (let ((p1-lines (cnesl-exp p1 lprec))
	(p2-lines (cnesl-exp p2 rprec))
	(op-line (new-line op)))
    (cond-wrap-paren 
     (> currentprec (min lprec rprec))
     (append-lines-2 (append-lines p1-lines op-line) p2-lines 50))))

(defun cnesl-pair (p1 p2 precedence)
  (cnesl-infix ", " p1 p2 precedence 1 0))

(defun cnesl-fn (p1 p2 precedence)
  (cnesl-infix " => " p1 p2 precedence 2 1))

(defun cnesl-closure (p1 p2 precedence)
  (declare (ignore p1 p2 precedence))
  (new-line "closure"))

(defun cnesl-binary-op (name binop left right precedence)
  (cnesl-infix (or (third binop) (format nil " ~(~a~) " name))
	       left right precedence 
	       (1- (second binop)) (second binop)))

(defun cnesl-unary-op (name op exp precedence)
  (cond-wrap-paren (>= precedence 8)
		   (append-lines (new-line (string (or (second op) name)))
				 (cnesl-exp exp 8))))

(defun cnesl-func (name exp precedence)
  (let* ((realname name)
	 (binary-op (and (atom name) (assoc realname *binops*)))
	 (unary-op (and (atom name) (assoc realname *unaryops*))))
    (cond
     (binary-op
      (cnesl-binary-op realname binary-op (second exp) (third exp) precedence))
     (unary-op
      (cnesl-unary-op realname unary-op exp precedence))
     (t (let* ((parallel? (and (listp name) (eql (car name) 'parallel)))
	       (fun (if parallel? (cdr name) name)))
	  (append-lines
	   (cnesl-exp fun 0) ;; if did'nt wrap, this should be an 8.
	   ;; Always wrap arguments in parens--not necessary, but looks better.
	   (cond-wrap-paren t (cnesl-exp exp 0))))))))

(defun cnesl-get-seq-exps (exp)
  (if (eql-pscode (car exp) 'seq_dist)
      (list (second (second exp)))
    (append (cnesl-get-seq-exps (second (second exp)))
	    (list (third (second exp))))))

(defun cnesl-seq-elts (exp-list)
  (let ((exp-lines (cnesl-exp (car exp-list) 1)))
    (if (= (length exp-list) 1) exp-lines
      (append-lines exp-lines (new-line ", ") 
		    (cnesl-seq-elts (cdr exp-list))))))

(defun cnesl-seq (exp)
  (let ((exps (cnesl-get-seq-exps exp)))
    (append-lines (new-line "[") (cnesl-seq-elts exps) (new-line "]"))))

(defun cnesl-elt (exp)
  (let ((left-lines (cnesl-exp (second (second exp))))
	(right-lines (cnesl-exp (third (second exp)))))
    (append-lines left-lines (new-line "[") right-lines (new-line "]"))))

(defun cnesl-iseq (args)
  (let ((start-lines (cnesl-exp (first-pair args)))
	(stride-lines (cnesl-exp (second-pair args)))
	(end-lines (cnesl-exp (third-pair args)))
	(cline (new-line ":")))
    (append-lines (new-line "[") start-lines cline end-lines 
		  cline stride-lines (new-line "]"))))

(defun cnesl-vector (exp)
  (cnesl-func (first exp) (second exp) 0))



(defun cnesl-print-data (self &optional (paren-p nil) type)
;  (format t "self ~a : type ~a~%" self type)
  (cond ((nesl-seq-p self)
	 (cnesl-print-nested-sequence self type))
	((numberp self) (format nil "~a" self) )
;	((nesl-struct-p self)
;	 (cnesl-print-nesl-struct self paren-p))
	((or (typep self 'closure) (typep self 'compiled-function)
	     (and (symbolp self) (fboundp self)) 
	     (and (consp self) (eql (car self) 'lambda)))
	 (format nil "fn"))
	((characterp self) (format nil "`~a" self))
	((eql self 'nil) (format nil "F"))
	((datatype-p self) 
	 (let ((val (datatype-value self)))
	   (if (consp val) (format nil "~(~a~)(~a)" (datatype-name self) 
			      (cnesl-print-data (datatype-value self) nil 
					(car (datatype-arg-type self))))
	     (format nil "~(~a~)(~a)" (datatype-name self) 
		     (cnesl-print-data (datatype-value self) paren-p)))))
	((atom self) (format nil "~a" self))
	((listp self)    
	 (format nil (if paren-p "(~a, ~a)" "~a, ~a")
		 (cnesl-print-data (first self) t (second type))
		 (cnesl-print-data (second self) nil (third type))))
	(t (nesl-error "Internal error: trying to print invalid type"))))

(defun cnesl-print-nested-sequence (self type &optional full)
  (declare (special *max-print-length*))
;  (format t "value ~a, type ~a~%" self type)
  (let ((data (coerce (nesl-seq-value self) 'list))
	(type (second type)))
    (if (eql type 'char)
	(format nil "~s" (coerce data 'string))
      (let ((ndata (mapcar #'(lambda (a) (cnesl-print-data a t type))
			   (if (or full (<= (length data) *max-print-length*))
			       data
			     (subseq data 0 *max-print-length*)))))
	(if (= (length data) 0)
	    ;; Can't print type since type is in wrong format (expanded)
	    (format nil "[]")
	  (format nil (cond ((and (not full) (= *max-print-length* 0)) "[...]")
			    ((or full 
				  (<= (length data) *max-print-length*))
			     "[~a~{, ~a~}]")
			    (t "[~a~{, ~a~},...]"))
		  (car ndata) (cdr ndata)))))))

(defun cnesl-print-nesl-struct (self &optional (paren-p nil))
  (let* ((data (nesl-struct-data self))
	 (type (nesl-struct-type self)))
    (if (eql type 'pair)
	(format nil (if paren-p "(~a, ~a)" "~a, ~a")
		(cnesl-print-data (first data) t)
		(cnesl-print-data (second data)))
      (format nil "~(~a~)(~a)" type (cnesl-print-data (car data))))))

(defun eql-pscode (val1 pscode-name)
  (and (symbolp val1) (eql val1 pscode-name)))

(defun cnesl-exp (a &optional (precedence 0))
  (cond ((atom a) 
	 (new-line (cond ((characterp a) (format nil "`~a" a))
			 ((symbolp a) (string-downcase a))
			 ((stringp a) (format nil "~s" a))
			 (t (format nil "~(~s~)" a)))))
	((listp a) 
	 (cond ((eql (car a) 'let)
		(cnesl-let a precedence))
	       ((eql (car a) 'over)
		(cnesl-over (second a) (third a)))
	       ((and (eql-pscode (car a) 'pack)
		     (listp (second a))
		     (eql (car (second a)) 'over))
		(cnesl-over (second (second a))
			    (second (third (second a)))
			    (third (third (second a)))))
	       ((or (eql (car a) 'vector)
		    (eql-pscode (car a) 'vector))
		(cnesl-vector a))
	       ((eql (car a) 'if)
		(cnesl-if (second a) (third a) (fourth a) precedence))
	       ((eql (car a) 'pair)
		(cnesl-pair (second a) (third a) precedence))
	       ((eql (car a) '=>)
		(cnesl-fn (second a) (third a) precedence))
	       ((eql (car a) 'closure)
		(cnesl-closure (second a) (third a) precedence))
	       ((or (eql-pscode (car a) 'make_sequence)
		    (eql-pscode (car a) 'seq_dist))
		(cnesl-seq a))
	       ((and (eql-pscode (car a) 'elt)
		     (eql (car (second a)) 'pair))
		(cnesl-elt a))
	       ((eql-pscode (car a) 'iseq)
		(cnesl-iseq (second a)))
	       (t
		(cnesl-func (first a) (second a) precedence))))))

(defun cnesl-type (type parenth? &optional (name-p nil))
  (cond ((atom type)
	 (new-line (string-downcase type)))
	((listp type)
	 (cond ((eql (first type) 'vector)
		(append-lines 
		 (new-line "[") (cnesl-type (second type) 1 name-p) 
		 (new-line "]")))
	       ((eql (first type) 'function)
		(let* ((func-type (second type))
		       (name 
			(if (symbolp func-type) func-type
			  (func-name (binding-original-code func-type))))
		       (func-symbol 
			(if name-p (format nil " (~a)-> " name) " -> ")))
		  (cond-wrap-paren 
		   (> parenth? 2)
		   (append-lines (cnesl-type (fourth type) 3 name-p)
				 (new-line func-symbol)
				 (cnesl-type (third type) 2 name-p)))))
	       ((eql (first type) 'pair)
		(cond-wrap-paren 
		 (plusp parenth?)
		 (append-lines (cnesl-type (second type) 1 name-p)
			       (new-line ", ")
			       (cnesl-type (third type) 0 name-p))))
	       (t 
		(let ((funlines (new-line (string-downcase (car type)))))
		  (if (eql (length type) 1)
		      funlines
		    (append-lines funlines (new-line "(")
				  (cnesl-type (second type) 1 name-p)
				  (cnesl-type-list (cddr type) name-p)
				  (new-line ")")))))))
	(t (nesl-error "Invalid type ~a" type))))

(defun cnesl-type-list (type name-p)
  (if (null type) (new-line "")
    (append-lines (new-line ", ") (cnesl-type (car type) 1 name-p)
		  (cnesl-type-list (cdr type) name-p))))

(defun cnesl-typebinds (bindlist)
  (let* ((bind (car bindlist))
	 (bindlines (new-line 
		     (format nil "~(~a in ~a~)"(first bind) (second bind)))))
    (if (= (length bindlist) 1) bindlines
      (append-lines bindlines 
		    (new-line "; ")
		    (cnesl-typebinds (cdr bindlist))))))

(defun cnesl-full-type (type &optional (name-p nil))
;  (format t "type ~a~%" type)
  (let ((type-lines (cnesl-type (car type) 0 name-p))
	(context-lines 
	 (if (cdr type)
	     (append-lines (new-line " :: ") (new-line "(") 
			   (cnesl-typebinds (cdr type)) (new-line ")"))
	   (new-line ""))))
    (append-lines type-lines context-lines)))

;;(defun cnesl-type-list (typelist)
;;  (if (= (length typelist) 1) 
;;      (cnesl-type (car typelist) 1)
;;    (append-lines (cnesl-type (car typelist) 1)
;;		  (cnesl-type-list (cdr typelist)))))

(defun cnesl-function (function)
  (let ((arg-lines (cnesl-exp (cons (func-name function)
				    (func-arguments function))))
	(body-lines (cnesl-exp (func-body function)))
	(type-lines (if (func-type function)
			(append-lines (new-line " : ") 
				      (cnesl-full-type (func-type function)))
		      (new-line ""))))
    (append (append-lines (new-line "function ") arg-lines 
			  type-lines (new-line " ="))
	    (inc-lines body-lines 2))))

(defun cnesl-datatype (arguments context)
  (let ((type-lines (cnesl-type arguments 0))
	(context-lines (if context 
			   (append-lines (new-line " :: ")
					 (cnesl-typebinds context))
			 (new-line ""))))
    (append-lines (new-line "datatype ") type-lines context-lines)))

(defun cnesl-assignment (assign)
  (append-lines (cnesl-exp (second assign))
		(new-line " = ")
		(cnesl-exp (third assign))))

(defun cnesl-toplevel (exp)
  (append-lines
   (cond ((func-p exp) 
	  (cnesl-function exp))
	 ((listp exp)
	  (cond ((eql (car exp) 'assign)
		 (cnesl-assignment exp))
		((eql (car exp) 'defrec)
		 (cnesl-datatype (second exp) (cddr exp)))
		(t (cnesl-exp exp))))
	 (t (cnesl-exp exp)))
   (new-line ";")))

(defun pprint-nesl (a &optional (stream t))
  (let ((exp-str (cnesl-toplevel a)))
    (dolist (line exp-str)
      (write-char #\newline stream)
      (dotimes (i (car line)) (write-char #\space stream))
      (write-string (cdr line) stream)
      )
    (force-output stream)))

(defun pprint-nesl-string-rec (lines indent stream max-lines)
  (if (null lines) ""
    (if (= 0 max-lines) (format stream "~%  .....")
      (concatenate 
       'string 
       (let* ((line (car lines))
	      (nindent (+ (car line) indent)))
	 (format stream "~%~a~a" 
		 (make-sequence 'string nindent :initial-element #\space)
		 (cdr line)))
       (pprint-nesl-string-rec (cdr lines) indent stream (- max-lines 1))))))

(defun pprint-oneline-string (a)
  (let ((str (pprint-nesl-string-rec a 0 nil -1)))
    (subseq str 1 (length str))))

(defun pprint-nesl-string (exp &optional (indent 0))
  (pprint-nesl-string-rec (cnesl-exp exp) indent nil  -1))

(defun pprint-nesl-short-string (exp)
  (pprint-oneline-string (cnesl-exp exp)))

(defun pprint-nesl-result (val type vars)
  (pprint-nesl-string-rec 
   (append-lines (cnesl-exp vars 1)
		 (new-line " = ")
		 (new-line (cnesl-print-data val t (car type)))
		 (new-line " : ")
		 (cnesl-full-type type))
   0 nil -1))
