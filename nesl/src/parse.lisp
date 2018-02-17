(in-package :nesl-lisp)

;;;;;;;;;;;
;;; The first part of this file is a general purpose operator precedence
;;; parser.  It is motivated by the CGOL parser by Pratt
;;;
;;;	    Pratt, Vaughan R., ``Top Down Operator Precedence,'' 
;;;	    ACM Symposium on Principles of Programming Languages 
;;;	    Boston, MA; October, 1973.                           
;;;
;;; The rest of the file contains Nesl specific syntax definitions.
;;;;;;;;;;;;

(eval-when (compile) (proclaim '(special token)))

(defun advance () 
  (let ((toke (cgoltoken)))
    (when (eql toke :exit) (nesl-error "Premature end of file."))
    (setq token toke)))

(defun verify (den) 
  (if den (progn (advance) den)))

(defun getden (indl)
  (and (symbolp token) (get token indl)))

(defun nud ()
  (verify
   (or (getden 'neslnud)
       (if (getden 'nesllbp)
	   (if (getden 'neslled)
	       (cgolerr (format nil "~a is missing its left argument." token))
	     (cgolerr (format nil "Unexpected ~a" token)))
	 (let ((val token)) #'(lambda () val))))))

(defun led () 
  (or (verify (getden 'neslled))
      #'(lambda (left) (list left (parse 30)))))

;      (cgolerr (format nil "~a is not an operator with a left argument."
;		       token))))

(defun parse (rbp)
  (do ((trans (funcall (nud)) (funcall (led) trans)))
      ((not (< rbp (or (getden 'nesllbp) 30))) 
       trans)))

(initialize-multi-character-token-table
    '|-+#&'()*,/:;<=>@[\\]^`{?\|}!$|)

(defmacro deftok (&rest a)
  (subst (list 'quote a) 'a '(mapc #'puttok a)))

(defun check-token (symbol)
  (if (eql token symbol) 
      (progn (advance) t)
    nil))

(defun check (symbol)
  (if (equal token symbol)
      (advance)
    (cgolerr (format nil "missing ~a before ~a." symbol token))))

(defun get-token ()
  (prog1 token (advance)))

(defun force-token (symbol error-message)
  (if (eql token symbol) 
      (progn (advance) t)
    (cgolerr (format nil error-message token))))

(defmacro defconstruct (name body)
  `(setf (get ',name 'neslnud)
      #'(lambda () ,body)))

(defmacro defconstruct-infix (name precedence body)
  `(progn
     (setf (get ',name 'neslled)
	   #'(lambda (left) ,body))
     (setf (get ',name 'nesllbp) ,precedence)))

(defun add-terminator (name)
  (setf (get name 'nesllbp) 0))

(defmacro add-infix-fun (op lprec rprec code)
  `(progn
    (setf (get ,op 'neslled) 
	  #'(lambda (left) (let ((right (parse ,rprec))) ,code)))
    (setf (get ,op 'nesllbp) ,lprec)))

(defun add-prefix (op precedence &optional trans)
  (let ((newop (or trans op)))
    (setf (get op 'neslnud) 
	  #'(lambda () (list newop (parse precedence))))))

;;;;;;;;;;;
;;; THE FOLLOWING IS NESL SPECIFIC
;;;;;;;;;;;

(defparameter *currently-parsing-type* nil)
(defparameter *funtype-hack* nil)

(defun end-token () (or (eql token '|;|) (eql token '|$|)))

(defun get-until-semicolon ()
  (if (end-token) nil (cons (get-token) (get-until-semicolon))))

(defun parse-toplevel ()
  (cond ((or (check-token 'help) (eql token '?)) 'help)
	((check-token 'exit) 'exitall)
	((check-token 'lisp) :exit)
	((check-token 'describe) (cons 'describe (get-until-semicolon)))
	((check-token 'apropos) (cons 'apropos (get-until-semicolon)))
	((check-token 'get) (cons 'get-background (get-until-semicolon)))
	((check-token 'set) (cons 'set (get-until-semicolon)))
	((check-token 'show) (cons 'show (get-until-semicolon)))
	((check-token 'shell) (cons 'shell (get-until-semicolon)))
	((check-token 'dump) 
	 (let ((com (get-token)))
	   (if (eql com 'vcode)
	       (list 'dump com (get-token) (parse 3))
	     (cons 'dump (cons com (get-until-semicolon))))))
	((check-token 'edit) (cons 'edit (get-until-semicolon)))
	((check-token 'load)
	 (if (end-token) (list 'load-nesl)
	   (list 'load-nesl 
		 (match-args (parse 3) 
			     (list-pair 'exp '(|:=| verbose t)
					'(|:=| print f)
					'(|:=| save_filename t))))))
	(t (parse -1))))

(setf (get '$ 'nesllbp) (- 1)) 

(setf (get '|;| 'nesllbp) (- 1)) 

(defconstruct |'| (get-token))

(defun add-infix (op precedence &optional trans)
  (let ((newop (or trans op)))
    (setf (get op 'neslled) 
	  #'(lambda (left) (list newop (list 'pair 
					     left (parse precedence)))))
    (setf (get op 'nesllbp) precedence)))

(defun parsecontext ()
  (let* ((typevar (get-token))
	(c (force-token 
	    'in 
	    "Expected an IN instead of ~a when defining a type context."))
	(typeclass (get-token))
	(context (list typevar typeclass)))
    (declare (ignore c))
    (if (check-token '|;|)
	(cons context (parsecontext))
      (list context))))

(defun fix-type-hack (type context)
  (declare (special *definitions* *funtype-hack*))
  (cons (listify-type type *definitions*) 
	(append context *funtype-hack*)))

(defun parsetype ()
  (let ((*currently-parsing-type* t)
	(*funtype-hack* nil))
    (declare (special *currently-parsing-type* *funtype-hack*))
    (let ((type (parse 2))
	  (context (if (check-token '|::|)
		       (prog2 (check-token '|(|)
			      (parsecontext)
			      (check-token '|)|))
		     nil)))
      (fix-type-hack type context))))

(defconstruct if
  (let ((condition (parse 3))
	(c1 (force-token 
	    'then "Expected a THEN instead of ~a in an IF construct."))
	(then-part (parse 3))
	(c2 (force-token 
	    'else "Expected an ELSE instead of ~a in an IF construct."))
	(else-part (parse 3)))
    (declare (ignore c1 c2))
    `(if ,condition ,then-part ,else-part)))

(defun parse-let-bindings ()
  (let* ((pattern (parse 3))
	 (c (force-token 
	     '=  "Expected an = instead of ~a after the pattern ~
                        in a LET binding."))
	 (expression (parse 3)))
    (declare (ignore c))
    (if (and (check-token '|;|) (not (eql token 'in)))
	(cons (list pattern expression) (parse-let-bindings))
      (list (list pattern expression)))))

(defun zip-let-bindings (bindings body)
  (if (null bindings) body
    (list 'let 
	  (car bindings) 
	  (zip-let-bindings (cdr bindings) body))))

(defconstruct let
  (let ((bindings (parse-let-bindings)))
    (force-token 
     'in 
     "Expected a ; or IN instead of ~a after a binding of a LET construct.")
    (zip-let-bindings bindings (parse 3))))

(defun strcat (a b)
  (concatenate 'string a b))

(defun none? (a)
  (if (eql a 'none) nil a))

(defconstruct defconfig
  (let ((config (parse 3)))
    (declare (special *nesl-path*))
    (let* ((defaults 
	     (list-pair 
	      'name
	      '(|:=| memory_size 1048576)
	      `(|:=| interp_file ,(strcat *nesl-path* "bin/vinterp.serial"))
	      '(|:=| machine_name "")
	      '(|:=| rsh_command "")
	      `(|:=| plot_file ,(strcat *nesl-path* "bin/xneslplot"))
	      '(|:=| max_time -1)
	      '(|:=| arguments "")
	      '(|:=| temp_dir "/tmp/")
	      `(|:=| background_command "background-unix")
	      `(|:=| foreground_command "foreground-unix")))
	   (alist (flatten-pair (match-args config defaults))))
      (list 'config 
	    (cons (nth 0 alist) 
		  (make-config :name (nth 0 alist)
			       :memory-size (nth 1 alist)
			       :interp-file (nth 2 alist)
			       :machine-name (nth 3 alist)
			       :rsh-command (none? (nth 4 alist))
			       :plot-file (nth 5 alist)
			       :max-time (nth 6 alist)
			       :arguments (nth 7 alist)
			       :temp-dir (nth 8 alist)
			       :background-command (nth 9 alist)
			       :foreground-command (nth 10 alist)))))))

(defconstruct function
  (let* ((funname (get-token))
	 (*current-fundef* funname)
	 (arguments (parse 30))
	 (argument (list (strip-defaults arguments)))
	 (type (if (check-token '|:|) (parsetype) nil))
	 (doc (if (stringp token) (get-token))))
    (declare (special *current-fundef* *default-list*))
    (when (and type (not (is-function-type? type)))
      (cgolerr "Not a function type."))
    (if (and type (end-token))
	(make-func :name funname :arguments argument :type type :body :stub)
      (progn 
	(force-token 
	 '= 
	 (if type 
      "Expected an = instead of ~a after the type declaration of a function."
      "Expected an = or : instead of ~a after the parameters of a function."))
	(let ((body (parse 3)))
	  (setq *default-list* (remove funname *default-list* :key #'car))
	  (when (has-defaults? arguments)
	    (push (cons funname arguments) *default-list*))
	  (make-func :name funname
		     :arguments argument
		     :type type
		     :body body
		     :other (list :documentation doc)))))))

(defconstruct datatype
  (let* ((typename (get-token))
	 (*current-typedef* typename)
	 (type  (parsetype)))
    (declare (special *current-typedef*))
    (cons 'defrec (cons (list typename (car type)) (cdr type)))))

(defun parse-empty-sequence ()
  (let ((*currently-parsing-type* t))
    (declare (special *currently-parsing-type* *definitions*))
    (make-empty-vector (listify-type (parse 29) *definitions*) *definitions*)))

(defun parse-integer-sequence (val1)
  (let ((val2 (parse 3))
	(val3 (if (check-token '|:|) (parse 3) 1))
	(c (force-token 
	    '] 
	    "Expected a ] instead of ~a to terminate an integer sequence.")))
    (declare (ignore c))
    (list (get-pscode 'iseq)
	  `(pair ,val1 (pair ,val3 ,val2)))))

(defun parse-sequence-list (depth current-list)
  (if (> depth 100)
      (cgolerr "The maximum length for a sequence in the [a,b,c,...] format is 100.
For larger sequences use one of read_object_from_file, read_int_seq_from_file,
or read_float_seq_from_file.")
    (if (check-token '|,|)
	(parse-sequence-list 
	 (+ depth 1)
	 (list (get-pscode 'make_sequence)
	       `(pair ,current-list ,(parse 4))))
      (progn 
	(force-token 
	 '] "Expected a ] instead of ~a to terminate a sequence.")
	current-list))))

(defconstruct [ 
  (if *currently-parsing-type*
      (prog1 (list 'vector (parse 29)) (check ']))
    (if (check-token '])
	(parse-empty-sequence)
      (let ((val1 (parse 4)))
	(if (check-token '|:|)
	    (parse-integer-sequence val1)
	  (parse-sequence-list 
	   1 (list (get-pscode 'seq_dist) `(pair ,val1 1))))))))

(defun parse-apply-to-each-binds ()
  (if (check-token '|;|)
      (let* ((bind-pattern (parse 3))
	     (bind-exp 
	      (if (check-token 'in) (parse 3) 
		(if (symbolp bind-pattern)
		    bind-pattern
		  (cgolerr "Invalid binding in an APPLY-TO-EACH construct")))))
	(cons (list bind-pattern bind-exp) (parse-apply-to-each-binds)))))
	    
(defun zip-over (bindings)
  (if (= (length bindings) 1)
      (car bindings)
    (let ((zipped (zip-over (cdr bindings)))
	  (bind (car bindings)))
      `((pair ,(first bind) ,(first zipped))
	(,(get-pscode 'zip-over)
	 (pair ,(second bind) ,(second zipped)))))))

(defun serial-conv-over (over-exp flag)
  (if (not flag) over-exp
    (let ((var (first (second over-exp)))
	  (ex1 (second (second over-exp)))
	  (body (third over-exp)))
      `(let (map-func (=> ,var ,body))
	 (let (exp-val ,ex1)
	   (,(get-pscode 'map) (pair map-func exp-val)))))))

(defconstruct {
  (let* ((sflag (check-token '*))
	 (body (parse 3))
	 (body? (check-token '|:|))
	 (first-bind-pattern (if body? (parse 3) body))
	 (first-bind-exp 
	  (if (check-token 'in) (parse 3) 
	    (if (symbolp first-bind-pattern)
		first-bind-pattern
	      (cgolerr "Invalid binding in an APPLY-TO-EACH construct."))))
	 (binding (zip-over 
		   (cons (list first-bind-pattern first-bind-exp)
			 (parse-apply-to-each-binds))))
	 (sieve (if (check-token '|\||) (parse 3) nil))
	 (c (force-token 
	     '} 
	     "Expected a } instead of ~a to terminate an APPLY-TO-EACH."))
	 (main (serial-conv-over 
		`(over ,binding
		       ,(if sieve `(pair ,body ,sieve) body))
		sflag)))
    (declare (ignore c))
    (if sieve
	(list (get-pscode 'pack) main)
      main)))

(defconstruct-infix [ 30
  (prog1
      (list (get-pscode 'elt) `(pair ,left ,(parse 3)))
    (force-token 
     ']
     "Expected ] instead of ~a to terminate a sequence reference.")))

(defconstruct |(|
  (let ((exp (parse 3)))
    (force-token 
     '|)|
     "Expected ) instead of ~a to terminate a parenthesized expression.")
    exp))

(defconstruct pair
  (cgolerr "PAIR is a reserved word.  Use , instead."))

(defconstruct-infix |(| 30
  (prog1
      (if (eql left 'time)
	  ;; A hack that macro expands time
	  `(let (foo-time (,(get-pscode 'start_timer) 0))
	     (pair ,(parse 3) (,(get-pscode 'stop_timer) 0)))
	(let () 
	  (declare (special *default-list*))
	  (let ((defaults (cdr (assoc left *default-list*))))
	    (when (eql token '|)|)
	      (cgolerr "Functions must have at least one argument."))
	    (if defaults
		(list left (match-args (parse 3) defaults))
	      (list left (parse 3))))))
    (check '|)|)))

(defconstruct closure
  (let ((foo (parse 3)))
    (when (not (and (listp foo) (eql (car foo) 'pair)))
      (cgolerr "Bad input for CLOSURE"))
    (list 'closure (second foo) (third foo))))
	
(deftok ->)
(deftok >>)
(deftok =>)
(deftok <-)
(deftok |::|)
(deftok ++)
(deftok /=)
(deftok ==)
(deftok <=)
(deftok >=)
(deftok &=)
(deftok |:=|)
(deftok |\|\||)

(add-terminator '|)|)
(add-terminator '])
(add-terminator '})
(add-terminator 'then)
(add-terminator 'else)
(add-terminator 'in)
(add-terminator '|:|)
(add-terminator '|\||)
(add-terminator '|::|)

;; This is a real hack since -> has 2 meanings (function type and read)
(add-infix-fun '-> 24 23
	       (if *currently-parsing-type*
		   (let ((typename (gensym)))
		     (push (list typename 'any) *funtype-hack*)
		     (list 'function typename right left))
		 (list '-> (list 'pair left right))))

(add-infix-fun '|,| 4 3 (list 'pair left right))
(add-infix-fun '>> 3 3 (list '>> left right))
(add-infix-fun '= 2 2 (list 'assign left right))
(add-infix-fun '|:=| 5 5 (list '|:=| left right))
(add-infix-fun '&= 2 2
  (list 'background left 
	(match-args 
	 right (list-pair 'exp '(|:=| mem default) 
			  '(|:=| max_time default)
			  '(|:=| config default)))))

(add-infix-fun '=> 5 4 (list '=> left right))

(add-infix '<- 23)
;;(add-infix '-> 24)
(add-infix '++ 20)

(add-prefix '|#| 25)
(add-prefix '@ 25)
(add-prefix '- 25 'negate)

(add-infix 'nand 8)
(add-infix 'and 8)
(add-infix 'or 7)
(add-infix 'nor 7)
(add-infix 'xor 7)
(add-infix '== 10)
(add-infix '/= 10)
(add-infix '< 10)
(add-infix '> 10)
(add-infix '<= 10)
(add-infix '>= 10)
(add-infix '+ 20)
(add-infix '- 20)
(add-infix '* 21)
(add-infix '/ 21)
(add-infix '|\|\|| 21)

(add-infix-fun '^ 22 22
	       (if (eq right 2) 
		   `(power-2 ,left)
		 (if (eq right 3) 
		     `(power-3 ,left) 
		   `(^ (pair ,left ,right)))))

(deftok |\|=|)

(add-infix-fun '|\|=| 2 2
  (let () 
    (declare (special *current-config*))	       
    (let ((filename (format nil "~anesl_~a_~a"
			(config-temp-dir *current-config*) 
			(user-name) (gensym))))
      `(file-assign (,left 
		    (let (r ,right)
		      (let (err (,(get-pscode 'write_object_to_file)
				 (pair r ,filename)))
			(,(get-pscode 'identity) r))))
		    ,filename))))

(defun pairp (a) (and (listp a) (eql (first a) 'pair)))
(defun defaultp (a) (and (listp a) (eql (first a) '|:=|)))
(defun car-pair (a) (if (pairp a) (second a) a))
(defun cdr-pair (a) (if (pairp a) (third a) nil))
(defun cddr-pair (a) (cdr-pair (cdr-pair a)))
(defun first-pair (a) (car-pair a))
(defun second-pair (a) (car-pair (cdr-pair a)))
(defun third-pair (a) (car-pair (cddr-pair a)))
(defun fourth-pair (a) (car-pair (cdr-pair (cddr-pair a))))
(defun cons-pair (a b) (if (null b) a (list 'pair a b)))
(defun list-pair (&rest args) (list-pair-r args))
(defun list-pair-r (args)  
  (if args (cons-pair (car args) (list-pair-r (cdr args))) nil))
(defun flatten-pair (a)
  (if (null a) a
    (cons (car-pair a) (flatten-pair (cdr-pair a)))))

(defun default-key (a)
  (if (not (defaultp a))
      (nesl-error "Bad format in an argument pattern.")
    (second a)))

(defun default-val (a)
  (if (not (defaultp a))
      (nesl-error "Bad format in an argument pattern.")
    (third a)))

(defun match-args (args defaults)
  (if (or (null defaults) (defaultp (car-pair defaults)))
      (progn 
	(check-defaults args defaults)
	(fill-defaults args defaults))
    (cons-pair (car-pair args) 
	       (match-args (cdr-pair args) (cdr-pair defaults)))))

(defun find-default (key tree)
  (if (null tree) nil
    (if (eql key (default-key (car-pair tree)))
	(default-val (car-pair tree))
      (find-default key (cdr-pair tree)))))

(defun fill-defaults (args defaults)
  (if (null defaults) nil
    (let ((val (or (find-default (default-key (car-pair defaults)) args)
	           (default-val (car-pair defaults)))))
      (cons-pair val (fill-defaults args (cdr-pair defaults))))))

(defun check-defaults (args defaults)
  (if (null args) nil
    (if (find-default (default-key (car-pair args)) defaults)
	(check-defaults (cdr-pair args) defaults)
      (nesl-error "Invalid optional argument ~a" 
		  (default-key (car-pair args))))))

(defun strip-defaults (args)
  (if (null args) nil
    (cons-pair  (if (defaultp (car-pair args)) 
		    (default-key (car-pair args))
		  (car-pair args))
		(strip-defaults (cdr-pair args)))))

(defun has-defaults? (args)
  (if (null args) nil
    (or (defaultp (car-pair args)) (has-defaults? (cdr-pair args)))))

