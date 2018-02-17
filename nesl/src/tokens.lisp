(in-package :nesl-lisp)

(eval-when (compile)
  (proclaim '(special standard-input)))

(defvar *begin-comment-char*  #\%)
(defvar *end-comment-char*  #\%)
(defvar token nil)
(defvar ctoken-table nil)
(defvar free-kons nil)
(defvar *interactive-p*)
(defvar *line-num*)

(defun cgolread (stream interactive)
  (let ((standard-input stream)
	(token-history nil)
	(*interactive-p* interactive))
    (declare (special standard-input token-history *interactive-p* *eof*))
    (cond ((eq (setq token (cgoltoken)) :exit)
	   (setq *eof* nil)
	   :exit)
	  (t (prog1 
		 (parse-toplevel)
	       (when interactive (read-line stream nil)))))))

(defun clear-nesl-input ()
  (do ((token token (cgoltoken)))
      ((or (eql token '$) (eql token '|;|) (eql token :exit))))
  (when *interactive-p* (read-line standard-input nil)))
  
(defun cgolerr (message)
  (declare (special token-history))
  (let ((history (if (> (length token-history) 10)
		     (cons "...." (reverse (subseq token-history 0 10)))
		   (reverse token-history))))
    (clear-nesl-input)
    (if *interactive-p*
	(nesl-error "Syntax error at end of expression:~{ ~a~}~%~a"
		    history message)
      (nesl-error "Line ~a: Syntax error at end of expression:~{ ~a~}~%~a"
		  *line-num* history message))))

(defun ctyi () 
  (let ((c (read-char standard-input nil -1)))
    (when (eql c #\newline) (incf *line-num*))
    c))

(defun ctyipeek () (peek-char nil standard-input nil -1))

(defun cuntyi (c) 
  (when (eql c #\newline) (decf *line-num*))
  (unread-char c standard-input))
	 
(defmacro return-token (c l &optional (reversed-p t))
  `(progn ,(if c `(cuntyi ,c))
	  (return (make-token ,l ,reversed-p fixnum-p flonum-p))))

;; The tokenizer is a simple loop with the character TYI'd pushed on the
;; token buffer after a series of special cases are checked.
;; The boolean state variables could be replaced with predicates
;; that look back on what in in the buffer, however the present implementation
;; is highly straightforward.

(defun cgoltoken ()
  (declare (special token-history *eof*))
  (first (push
	  (do ((l nil (kons c l))
	       (c (cskip-whitespace) (ctyi))
	       (temp)
	       (fixnum-p nil)
	       (flonum-p nil)
	       (expt-p nil)
	       (digit-after-expt-p nil)
	       )
	      (nil)
	    (cond ((equal c -1)
		   (if (null l)
		       (progn (setq *eof* t) (return :exit))
		     (return-token c l)))
		  ((char= c #\!)
		           (if (null l)
			       (return (read standard-input nil :exit))
			     (return-token c l)))
		  ((char= c #\`)
        		   (if (null l)
			       (return (ctyi))
			     (return-token c l)))
		  ((char= c #\") 
		   (if (null l)
		       (return (ctoken-string))
		     (return-token c l)))
		  ((cwhitespacep c)
		   (return-token c l))
		  ((char= c #\.)
		   (cond ((null l)
			  (if (cdigit-p (ctyipeek))
			      (setq fixnum-p nil flonum-p t)
			    (return '\.)))
			 (fixnum-p
			  (setq fixnum-p nil flonum-p t))
			 (t (return-token c l))))
		  ((and (or (char= c #\E) (char= c #\e))
			(or fixnum-p flonum-p)
			(not expt-p))
		   (let ((p (ctyipeek)))
		     (if (not (or (char= p #\+)
				  (char= p #\-)
				  (cdigit-p p)))
			 (return-token c l)))
		   (setq fixnum-p nil flonum-p t)
		   (setq expt-p t))
		  ((cdigit-p c)
		   (if (null l)
		       (setq fixnum-p t))
		   (if expt-p (setq digit-after-expt-p t)))
		  ((and (or (char= c #\+) (char= c #\-))
			flonum-p
			expt-p
			(not digit-after-expt-p)
			(cdigit-p (ctyipeek))))
		  ((setq temp (assoc (setq c (char-upcase c)) ctoken-table))
		   (if (null l)
		       (return-token nil (kons c (cfollow-tail (cdr temp)))
				     nil)
		       (return-token c l)))
		  (t
		   (if (or fixnum-p flonum-p) (return-token c l)))))
	  token-history)))

(defun cwhitespacep (c)
  (or (member c '(#\Space #\Return #\Linefeed #\Tab #\Page))
      (eql c *begin-comment-char*)))

(defun cskip-whitespace ()
  (do ((commentp nil)(c))
      (nil)
      (setq c (ctyi))
      (cond ((eql c (if commentp *end-comment-char* *begin-comment-char*))
	     (setq commentp (not commentp)))
	    ((cwhitespacep c)
	     (when (and (eql c #\newline) *interactive-p*)
	       (format t "> ")))
	    (commentp 
	     (when (eql c -1)
	       (nesl-error "End of File reached inside of a comment.")))
	    (t (return c)))))

(defun initialize-multi-character-token-table (string)
  (setq ctoken-table
	(mapcar #'list (coerce (string string) 'list))))

(defun cfollow-tail (alist)
  ;; this way of recognizing tokens is taken from the original cgol,
  ;; is fast and easy and passes all tokens which are subtokens
  ;; of explicitely defined tokens.
  (if (null alist) nil
      (let ((c (char-upcase (ctyipeek))))
	(cond ((setq alist (assoc c alist))
	       (ctyi)
	       (kons c (cfollow-tail (cdr alist))))
	      (t
	       nil)))))

(defmacro with-working-cons (&rest l)
  (cond (nil
	 `(let ((default-cons-area working-storage-area))
	    ,@l))
	('else
	 `(progn ,@l))))

(defun puttok (token)
  ;; entry point for defining tokens.
  (with-working-cons
    (let ((l (coerce (string token) 'list)))
      (or (assoc (car l) ctoken-table)
	  (error "token with illegal first character" token))
      (setq ctoken-table (inserttok l ctoken-table)))))

(defun inserttok (tok toktable) 
  (if (null tok)
      toktable
      (let ((st (assoc (car tok) toktable)))
	(cond ((null st)
	       (cons (cons (car tok)
			   (inserttok (cdr tok) nil))
		     toktable))
	      (t
	       (rplacd st (inserttok (cdr tok) (cdr st)))
	       toktable)))))

(defun cgol-make-string (l)
  (prog1 (coerce (nreverse l) 'string) (reklaim l)))

(defun ctoken-string ()
  (declare (special *eof*))
  (do ((c (ctyi) (ctyi))
       (l nil (kons c l)))
      (nil)
      (cond ((eql c -1) 
	     (setq *eof* t) 
	     (nesl-error "Reached end-of-input while parsing a quoted string"))
	    ((char= c #\\)
	     (setq c (ctyi)))
	    ((char= c #\")
	     (return (cgol-make-string l))))))
	   
(defun cdigit-p (x)
 (not (or (char< x #\0) (char> x #\9))))

(defun make-token (l rp fixnum-p flonum-p)
  ;; takes the stack of characters and makes a token.
  (if rp (setq l (nreverse l)))
  (prog1
      (cond (fixnum-p (readlist l))
	    (flonum-p (coerce (readlist l) 'double-float))
	    (t (intern (string-upcase (coerce l 'string)))))
    (reklaim l)
    ))

(defun readlist (list) (read-from-string (coerce list 'string)))

;; Keeping our own free-list is a way to use lists for stacks without the
;; overhead of garbage collection. On the LISPM this should be replaced
;; with a STRING and an INDEX pointer.

(defun kons (kar kdr)
  (if free-kons
      (progn
	(rplaca free-kons kar)
	(rplacd (prog1 free-kons (setq free-kons (cdr free-kons)))
		kdr))
      (with-working-cons
	(cons kar kdr))))

(defun reklaim (l)
  (setq free-kons (nconc l free-kons)))
