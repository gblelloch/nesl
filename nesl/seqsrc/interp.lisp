;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)


(defparameter *curr-unused* nil)

; adds var-name as value and setqs var to value

(defun add-runtime-binding (e-left val &optional type name)
  (declare (special *defs*))
  (cond
   ((eql e-left 'pi) (nesl-error "PI is built-in, cannot be redefined."))
   ((member e-left '(t f)) 
    (nesl-error "~a is a special symbol, it cannot be used as a variable." 
		e-left))
   ((listp e-left) 
    (if (eql (first e-left) 'pair)
	(let ((a1 (second e-left)) (a2 (third e-left))
	      (t1 (list (second (first type)))) 
	      (t2 (list (third (first type)))))
	  (progn
	    (add-runtime-binding a1 (first val) t1 (first name)) 
	    (add-runtime-binding a2 (second val) t2 (second name))))
      (add-runtime-binding (second e-left) (datatype-value val)
		   (datatype-arg-type val) name)))
   (t 
    (let* 
	((binding (make-binding :name e-left :value name :type type 
			       :original-code (make-func))))
    (progn
      (eval `(if (not (boundp ',name)) (defvar ,name)))
      (eval `(setq ,name ,val))
      (eval `(setq *defs* (cons ,binding *defs*))))))))



(defun add-var-binding (e-left val defns &optional type)
  (cond
   ((eql e-left 'pi) (nesl-error "PI is built-in, cannot be redefined."))
   ((member e-left '(t f)) 
    (nesl-error "~a is a special symbol, it cannot be used as a variable." 
		e-left))
   ((listp e-left) 
    (if (eql (first e-left) 'pair)
	(let ((a1 (second e-left)) (a2 (third e-left))
	      (t1 (list (second (first type)))) 
	      (t2 (list (third (first type)))))
	  (add-var-binding a2 (second val) (add-var-binding a1 
						    (first val) defns t1) 
		       t2))
      (add-var-binding (second e-left) (datatype-value val) defns 
		   (datatype-arg-type val))))
   (t 
    (let* ((name (setq-var e-left val defns type))
	   (binding (make-binding :name e-left :value name :type type 
				  :original-code (make-func))))
      (cons binding defns)))))


(defun get-def (name defns)
  (if (null defns) (nesl-error "~a is undefined." name)
    (let ((first-def (first defns)))
	(if (eql name (binding-name first-def)) first-def
	  (get-def name (cdr defns))))))


(defun find-def (name defns)
  (if (null defns) nil
    (let ((first-def (first defns)))
      (if (eql name (binding-name first-def)) first-def
	(find-def name (cdr defns))))))



(defun compile-get-def (name defns)
  (if (null defns) nil
    (let ((first-def (first defns)))
      (if (eql name (binding-name first-def)) (binding-value first-def)
	(compile-get-def name (cdr defns))))))


(defun get-type-def (name defns)
  (cond 
   ((null defns) nil)
   (t (let ((first-def (first defns)))
	(cond ((and (eql name (binding-name first-def))
		    (binding-datatype first-def))
	       (binding-type first-def))
	      (t (get-type-def name (cdr defns))))))))




(defun make-empty-vect (type) 
  (make-nesl-seq :len 0 :value (vector) :type (list 'vector type)))


   
(defun nesl-constantp (v)
  (or   
   (and (not (binding-p v)) (constantp v) (not (eql v 'pi)))
   (and (listp v) (eql (car v) 'make-empty-vector))))
   

(defun eval-nesl-const (e)
  (cond 
   ((typep e 'string) 
    (let ((char-list (coerce e 'list)))
      `',(make-nesl-seq :len (length char-list)
		     :value (coerce char-list 'vector) 
		     :type '(vector char))))
   ((listp e) `',(make-empty-vect (second e)))
   (t e)))

(defun get-internal-names (lhs defs)
  (cond
   ((consp lhs)
    (if (eql (first lhs) 'pair) 
	(list (get-internal-names (second lhs) defs)
	      (get-internal-names (third lhs) defs))
      (get-internal-names (second lhs) defs)))
   (t (binding-value (get-def lhs defs)))))


(defun compile-assmt (lhs rhs defns old-defns outstr type)
  (declare (special *ready* *curr-checklist*))
  (let* ((body (compile-exp rhs defns nil))
	 (decls (declare-unused-special-vars nil rhs))
	 (specials (sift-out-old (cdr decls) defns old-defns))
	 (dummy-fn `(defun top-level-dummy-fn () ,body))
	 (is-datatype (and (consp body) (eq (first body) 'lambda))))
;;   (setq test dummy-fn)
    (unless is-datatype 
	    (eval dummy-fn)
	    (compile 'top-level-dummy-fn))
    (let* ((value (if is-datatype body (top-level-dummy-fn)))
	   (new-defs (add-var-binding lhs value defns type)))
      (when outstr
	    (progn (setq *curr-checklist* (nunion *curr-checklist* specials))
		   (add-to-lisp `(add-runtime-binding ',lhs ,body ',type
				   ',(get-internal-names lhs new-defs)))))
      (cons value new-defs))))



(defun eval-exp (exp defns)
  (declare (special *ready*))
  (if *ready* 
      (let* ((body (compile-exp exp defns nil))
	     (dummy-fn `(defun top-level-dummy-fn () ,body)))
	(eval dummy-fn)
	(compile 'top-level-dummy-fn)
	(top-level-dummy-fn))
    (let* ((body (compile-exp exp defns nil)))
	 (eval body))))

; Does not compile.
(defun init-eval-exp (exp defns)
  (declare (special *ready*))
  (let* ((body (compile-exp exp defns nil)))
    (eval body)))


(defun pattern-match (lhs rhs str)
  (let ((lhs-v (listify-pairs lhs)))
    (cond 
     ((member 'pi lhs-v) (nesl-error "PI is built-in, cannot be redefined."))
     ((member 't lhs-v)
      (nesl-error "T is a special symbol, it cannot be used as a variable."))
     ((member 'f lhs-v)
      (nesl-error "F is a special symbol, it cannot be used as a variable."))
     (t (do-pattern-match lhs rhs str))
)))


(defun do-pattern-match (lhs rhs str)
  (declare (special *curr-unused*))
  (cond 
   ((and
     (not (set-difference (listify-pairs lhs) *curr-unused*)) 
     (or (symbolp rhs) (and (consp rhs) 
			    (or (equal (car rhs) 'second) 
				(equal (car rhs) 'first)))))
    (progn
      (setq *curr-unused* (nset-difference *curr-unused* (listify-pairs lhs)))
      nil))
   ((and (consp rhs) (and (consp lhs) (eql (first lhs) 'pair))) 
    (let ((rhs-name (gentemp str)))
      (setq *curr-unused* (cons rhs-name *curr-unused*))
      (cons (list rhs-name rhs) (do-pattern-match lhs rhs-name str))))
   ((consp lhs) 
    (if (eql (first lhs) 'pair)
	(progn
	  (setq *curr-unused* (nset-difference *curr-unused* (list rhs)))
	  (append (do-pattern-match (second lhs) `(first ,rhs) str)
		  (do-pattern-match (third lhs) `(second ,rhs) str)))
      ; match datatype
      (do-pattern-match (second lhs) `(datatype-value ,rhs) str)))
   (t (list (list lhs rhs)))))



(defun declare-unused-special-vars (lhs exp)
  (let* ((lhs (listify-pairs lhs))
	 (free-vars (ext-exp exp nil))
	 (unused-vars (set-difference lhs free-vars))
	 (special-vars (set-difference free-vars lhs)))
    (cons unused-vars special-vars)))


(defun declare-unused-vars (lhs exp)
  (let* ((lhs (listify-pairs lhs))
	 (free-vars (ext-exp exp nil))
	 (unused-vars (set-difference lhs free-vars)))
    unused-vars ))
	


(defun compile-let (lhs rhs exp defns local-defs &optional str)
  (let ((*curr-unused* (declare-unused-vars lhs exp))) 
  (append (list 'let* (pattern-match lhs rhs (if str "o" "l")))
	  (if *curr-unused* `((declare ,(cons 'ignore *curr-unused*))) nil)
	  (list (compile-exp exp defns local-defs)))))


(defun compile-over (lhs rhs exp defns local-defs)
  (let ((new-rhs (if (consp rhs) 'o-rhs rhs))
	(first-binding (if (consp rhs) `((o-rhs ,rhs)) nil)))
    (list 'let* 
	  (append first-binding  
		  `((o-len (nesl-seq-len ,new-rhs))
		    (o-res (make-array o-len))
		    (o-val (nesl-seq-value ,new-rhs))))
	  `(do* ((o-i 0 (+ o-i 1)))
	       ((= o-i o-len) t)
	       (setf (svref o-res o-i) 
		     ,(compile-let lhs '(svref o-val o-i) 
				   exp defns local-defs t)))
	  `(make-nesl-seq :len o-len :value o-res))))
  
   

(defun compile-symbol (sym defns local-defs)
  (cond 
   ((member sym local-defs) sym)
   ((eql sym 'f) nil)
   (t (let ((bind (compile-get-def sym defns)))
	(if bind bind sym)))))


(defun compile-symlist (sym-list defns)
  (if (null sym-list) nil
    (let ((value (compile-get-def (car sym-list) defns)))
      (if (consp value) (compile-symlist (cdr sym-list) defns)
	(cons value (compile-symlist (cdr sym-list) defns))))))
 


(defun compile-exp (e defns local-defs) 
  (cond
   ((nesl-constantp e) (eval-nesl-const e))
   ((symbolp e) (compile-symbol e defns local-defs))
   (t
    (let ((etype (first e)))
      (cond
       ((or (eql etype 'let) (eql etype 'over))
	(let* ((lhs (first (second e)))
	       (rhs (compile-exp (second (second e)) defns local-defs)) 
	       (nlocal-defs (append local-defs 
				    (listify-pairs (first (second e)))))) 
	  (if (eql etype 'let) 
	      (compile-let lhs rhs (third e) defns nlocal-defs)  
	    (compile-over  lhs rhs (third e) defns nlocal-defs))))
       ((eql etype 'if) (list 'if (compile-exp (second e) defns local-defs)
			      (compile-exp (third e) defns local-defs)
			      (compile-exp (fourth e) defns local-defs)))
       ((eql etype 'pair) (list 'list (compile-exp (second e) defns local-defs)
				(compile-exp (third e) defns local-defs)))
       ((eql etype '=>) (compile-flet (second e) (third e) defns))
       (t (let ((fn (compile-exp (first e) defns local-defs))
		(arg (compile-exp (second e) defns local-defs)))
	    (if (or (consp (first e))
		    (member (first e) local-defs))
		(list 'funcall fn arg)
	      (list fn arg)
))))))))


;;; handles recursion

(defun add-func-def (fn defns type)
  (let ((arg (first (func-arguments fn)))
	(name (func-name fn))
	(body (func-body fn))
	(type (if type type (func-type fn))))
    (if (and (listp body) (eql (car body) :primitive))
	(let ((prim-name (second body)))
	  (progn
	    (setq-fn prim-name (boundp prim-name))
	    (cons (make-binding :name name :value prim-name
				:type type :original-code fn)
		  defns)))
      (let* ((old-def (find-def name defns))
	     (same-def (and old-def (equal (binding-type old-def) type)))
	     (new-name (if same-def (binding-value old-def)
			 (gentemp (concatenate 'string "internal-" 
						 (symbol-name name)))))
	     (arg-list (listify-pairs arg))
	     (decls (declare-unused-special-vars arg body))
	     (arg-name (if (consp arg) (gentemp "arg") arg))
	     (*curr-unused* (car decls))
	     (ig-decl (if (set-difference arg-list *curr-unused*) nil
			  `((declare ,(list 'ignore arg-name)))))
	     (decls ig-decl)
	     (new-binding (make-binding :name name
					:value new-name
					:type type :original-code fn))
	     (new-defns (cons new-binding defns))
	     (fn-body (compile-exp body new-defns arg-list))
	     (new-body (list 'defun new-name (list arg-name)))
	     (arg-bind (if (consp arg) (pattern-match arg arg-name "a")
			 nil))
	     (new-body (if arg-bind 
			   (append new-body 
				   (list (append (list 'let* 
						   (pattern-match arg arg-name
							     "a"))
						 decls (list fn-body))))
			   (append new-body decls (list fn-body)))))
	(progn 
	  (eval new-body)
	  (compile new-name)
	  (setq-fn new-name same-def)
	  (if (and old-def (not same-def)) 
	      (format t "Redefining ~a with a new type. Old calls will not be modified.~%" name))
	  new-defns)))))

(defun search-in-range (head-list stop name)
  (cond ((eq head-list stop) nil)
	((null head-list) nil)
	(t (let ((first-def (car head-list)))
	     (if (eql (binding-name first-def) name) t
	       (search-in-range (cdr head-list) stop name))))))


(defun sift-out-old (specials defns old-defns)
  (if (null specials) nil
    (let ((name (car specials)))
      (if (search-in-range defns old-defns name)
	  (sift-out-old (cdr specials) defns old-defns)
      (cons name (sift-out-old (cdr specials) defns old-defns))))))



(defun add-and-dump-func-def (fn defns type old-defns)
  (declare (special *curr-checklist*)) 
  (let ((arg (first (func-arguments fn)))
	(name (func-name fn))
	(body (func-body fn))
	(type (if type type (func-type fn))))
    (if (and (listp body) (eql (car body) :primitive))
	(let* ((prim-name (second body))
	       (new-binding (make-binding :name name :value prim-name
					  :type type :original-code fn)))
	  (progn
;;
;;	    (add-to-lisp `(format t ">>>~a..." ',prim-name))
	    (when (not (boundp prim-name)) 
		  (add-to-lisp `(defvar ,prim-name))
		  (add-to-lisp `(setq ,prim-name (function ,prim-name))))
	    (setq-fn prim-name (boundp prim-name))
	    (add-to-lisp `(setq *defs* (cons ,new-binding *defs*)))
	    (cons new-binding defns)))
      (let* ((old-def (find-def name defns))
	     (same-def (and old-def (equal (binding-type old-def) type)))
	     (new-name (if same-def (binding-value old-def)
			 (gentemp (concatenate 'string "internal-" 
						 (symbol-name name)))))
	     (arg-list (listify-pairs arg))
	     (decls (declare-unused-special-vars arg body))
	     (arg-name (if (consp arg) (gentemp "arg") arg))
	     (*curr-unused* (car decls))
	     (ig-decl (if (set-difference arg-list *curr-unused*) nil
			  `((declare ,(list 'ignore arg-name)))))
	     (specials (cdr decls))
	     (decls ig-decl)
	     (new-binding (make-binding :name name
					:value new-name
					:type type :original-code fn))
	     (new-defns (cons new-binding defns))
	     (specials (sift-out-old specials new-defns old-defns))
	     (fn-body (compile-exp body new-defns arg-list))
	     (new-body (list 'defun new-name (list arg-name)))
	     (arg-bind (if (consp arg) (pattern-match arg arg-name "a")
			 nil))
	     (new-body (if arg-bind 
			   (append new-body 
				   (list (append (list 'let* arg-bind)
						 decls (list fn-body))))
			 (append new-body decls (list fn-body)))))
	(progn 
	  (setq *curr-checklist* (nunion *curr-checklist* specials))
	  (add-to-lisp new-body)
	  (if (not (boundp new-name)) (add-to-lisp `(defvar ,new-name)))
	  (add-to-lisp `(setq ,new-name (function ,new-name)))
	  (add-to-lisp `(setq *defs* (cons ,new-binding *defs*)))
	  (eval new-body)
	  (compile new-name)
	  (setq-fn new-name same-def)
	  (if (and old-def (not same-def)) 
	      (format t "Redefining ~a with a new type. Old calls will not be modified.~%" name))
	  new-defns)))))


(defun add-to-lisp (code)
  (declare (special *curr-lisp-code*))
  (setq *curr-lisp-code* (nconc *curr-lisp-code* (list code))))


(defun compile-flet (arg body defns)
  (let*
      ((fn-name (gentemp "F"))
       (arg-list (listify-pairs arg))
       (decls (declare-unused-special-vars arg body))
       (arg-name (if (consp arg) (gentemp "arg") arg))
       (fn-body (compile-exp body defns arg-list))
       (*curr-unused* (car decls))
       (ig-decl (if (set-difference arg-list *curr-unused*) nil
		  `((declare ,(list 'ignore arg-name)))))
       (arg-bind (if (consp arg) (pattern-match arg arg-name "a")
		   nil))
       (new-body (list 'defun fn-name (list arg-name)))
       (new-body (if arg-bind 
		     (append new-body 
			     (list (append (list 'let* 
						 (pattern-match arg arg-name
								"a"))
					   ig-decl (list fn-body))))
		   (append new-body ig-decl (list fn-body)))))
    (progn 
      (eval new-body)
      (compile fn-name)
      (setq-fn fn-name nil)
      fn-name)))


(defun add-datatype (exp defns outstr)
  (let* ((typename (second exp))
	 (typevars (fourth exp))
	 (typein (parse-general-type (third exp) typevars defns))
	 (typeout (cons typename (mapcar #'car typevars)))
	 (type (cons (list 'function nil typeout typein) typevars))
	 ;; Guy -- took away the compile since GCL does not like 
	 ;; dynamic compilation
	 (body `(lambda (a &optional b) (datatype-var ',typename ',type a b)))
	 (new-binding (make-binding 
		       :name typename :value body 
		       :type type :datatype t 
		       :original-code 
		       (make-func 
			:other (list :primitive t :documentation 
			  (format nil "Constructor for record type ~a"
				  typename))))))
    (if outstr (add-to-lisp `(setq *defs* (cons ,new-binding *defs*))))
    (cons new-binding defns)))


(defun typecheck-and-eval-exp (exp defns)
  (typecheck-top-exp exp defns)
  (eval-exp exp defns))


(defun setq-var (var-name value defns type)
  (let* ((old-def (find-def var-name defns))
	 (same-def (and old-def (equal (binding-type old-def) type)))
	 (new-name (if same-def (binding-value old-def)
		     (gentemp (concatenate 'string "internal-" 
					       (symbol-name var-name))))))
    (progn 
      (eval `(defun ,new-name (x) (funcall ,new-name x)))
      (if (not same-def) (eval `(defvar ,new-name)))
      (eval `(setq ,new-name ',value))
      (if (and old-def (not same-def) (not (eql var-name 'it)))
	  (format t "Redefining ~a with a new type. Old calls will not be modified.~%" var-name))
      new-name)))
  



(defun setq-fn (name same-def)
  (progn
    (if (not same-def) (eval `(defvar ,name)))
    (eval `(setq ,name (function ,name)))))


;; This function returns 4 values
;;   1) the new definitions
;;   2) the value of the expression
;;   3) the type of the expression
;;   4) the variable assigned by the expression
(defun eval-nesl-toplevel (exp defns &optional outstr old-defns)
;;  (format t "EXP = ~a~%" exp)
  (cond 
   ((func-p exp) 
    (let* ((*current-fundef* (func-name exp))
	   (type (typecheck-top-func exp defns))
	   (defs (if outstr 
		     (add-and-dump-func-def exp defns type old-defns)
		   (add-func-def exp defns type))))
      (declare (special *current-fundef*))
      (values defs (make-closure) type (func-name exp))))
   ((and (listp exp) (eql (first exp) 'ASSIGN)) 
    (let* ((type (typecheck-assign exp defns))
	   (defs-val (compile-assmt (second exp) (third exp) defns old-defns
				 outstr type)))
      (values (cdr defs-val) (car defs-val) type (second exp))))
   ((and (listp exp) (eql (first exp) 'DEFREC))
    (let ((name (second exp))
	  (defs (add-datatype exp defns outstr)))
      (values defs (make-closure) (binding-type (first defs)) name)))
   (t
    (let* ((type (typecheck-top-exp exp defns))
	  (defs-val (compile-assmt 'it exp defns old-defns outstr type)))
      (values (cdr defs-val) (car defs-val) type 'it)))))




(defun sym-name (name)
  (declare (special *defs*))
  (binding-value (get-def name *defs*)))


(defun write-to-str (code str)
  (if str (progn 
	    (print code str)
	    (terpri str))))


(defun set-bindings (blist)
  (dolist (vars blist)
	  (let ((val (car vars))
		(bind-val (cdr vars)))
	    (progn
	      (eval `(defvar ,val))
	      (eval `(setq ,val ,bind-val))))))


(defun check-types (typelist defs)
  (declare (special *defs* *bind-list*))
  (setq *bind-list* nil)
    (dolist (check-var typelist (set-bindings *bind-list*))
      (let* ((var (car check-var))
	     (rest (cdr check-var))
	     (type (car rest))
	     (val (cdr rest))
	     (bind (get-def var defs))
	     (bind-type (binding-type bind))
	     (bind-val (binding-value bind)))
	(if (and bind (equal bind-type type))
	    (if (and (symbolp val) (not (eql bind-val val)))
		(if (boundp val) 
		    (progn 
;;		      (format t "Check-types ~a: ~a vs ~a~%" var bind-val val)
		      (nload-error))
		  (let ((new-def (make-binding 
				  :name var :type type :value val 
				  :original-code (binding-original-code bind)
				  :datatype (binding-datatype bind))))
		    (setq *bind-list* (cons (cons val bind-val) *bind-list*))
		    (setq *defs* (cons new-def *defs*))
		    (when (and (listp (car type)) (eql (caar type) 'FUNCTION))
			  (eval `(defun ,val (x) (,bind-val x)))))))
	  (nesl-error "~a is not of type (~a)." var 
		      (cdar (cnesl-full-type type)))))))


(defun extract-types-values (var-list defs)
  (if (null var-list) nil
    (let* ((var (car var-list))
	   (defn (get-def var defs)))
      (cons (cons var (cons (binding-type defn) (binding-value defn)))
	    (extract-types-values (cdr var-list) defs)))))




#+(or cmu allegro) (defmethod make-load-form ((s binding) &optional foo)
	    (declare (ignore foo))
	    `(make-binding :name ',(binding-name s) :value ',(binding-value s) 
			   :type ',(binding-type s) 
			   :datatype ',(binding-datatype s)
			   :original-code ',(binding-original-code s)))

#+(or cmu allegro) (defmethod make-load-form ((s func) &optional foo)
	    (declare (ignore foo))
	    `(make-func :name ',(func-name s) :arguments ',(func-arguments s)
			:type ',(func-type s) :body ',(func-body s)
			:other ',(func-other s)))

#+(or cmu allegro) (defmethod make-load-form ((s nesl-seq) &optional foo)
	    (declare (ignore foo))
	    `(make-nesl-seq :value ',(nesl-seq-value s) :len ',(nesl-seq-len s)
			    :type ',(nesl-seq-type s)))

#+(or cmu allegro) (defmethod make-load-form ((s datatype) &optional foo)
	    (declare (ignore foo))
	    `(make-datatype :value ',(datatype-value s) 
			    :name ',(datatype-name s) 
			    :type ',(datatype-type s)
			    :arg-type ',(datatype-arg-type s)))




