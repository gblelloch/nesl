;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

(defun prim-nesl-run-error (a) 
  (nesl-runtime-error (coerce (nesl-seq-value a) 'string))) 

(defun prim-not (a) 
    (typecase a
	      (integer (lognot a))
	      (t (not a))))

(defun prim-or (c)
  (let ((a (first c))
	(b (second c)))
    (typecase a
	      (integer (logior a b))
	      (t (or a b)))))

(defun prim-and (c) 
  (let ((a (first c))
	(b (second c)))
    (typecase a
	      (integer (logand a b))
	      (t (and a b)))))

(defun prim-xor (c) 
  (let ((a (first c))
	(b (second c)))
    (typecase a
	      (integer (logxor a b))
	      (t (nesl-xor a b)))))

(defun prim-== (c)  
  (let ((a (first c))
	(b (second c)))
    (eql a b)))

(defun prim-/= (c) 
  (let ((a (first c))
	(b (second c)))
    (not (eql a b))))

(defun prim-< (c) 
  (let ((a (first c))
	(b (second c)))
    (if (characterp a) (char< a b) (< a b))))

(defun prim-> (c) 
  (let ((a (first c))
	(b (second c)))
    (if (characterp a) (char> a b) (> a b))))

(defun prim-<= (c)
  (let ((a (first c))
	(b (second c)))
    (if (characterp a) (char<= a b) (<= a b))))

(defun prim->= (c)
  (let ((a (first c))
	(b (second c)))
    (if (characterp a) (char>= a b) (>= a b))))

(defun prim-plusp (v) (plusp v))

(defun prim-minusp (v) (minusp v))

(defun prim-zerop (v) (zerop v))

(defun prim-oddp (v) (oddp v))

(defun prim-evenp (v) (evenp v))

(defun prim-+ (c)
  (let ((a (first c))
	(b (second c)))
    (+ a b)))

(defun prim-- (c)
  (let ((a (first c))
	(b (second c)))
    (- a b)))

(defun prim-negate (a) (- 0 a))

(defun prim-select (c)
  (let ((flag (first c))
	(rest (second c)))
    (if flag (first rest) (second rest))))

(defun prim-abs (a) (abs a))

(defun prim-max (c) 
  (let ((a (first c))
	(b (second c)))
    (max a b)))

(defun prim-min (c) 
    (let ((a (first c))
	(b (second c)))
      (min a b)))

(defun prim-* (c) 
  (let ((a (first c))
	(b (second c)))
    (* a b)))

(defun prim-/ (c) 
    (let ((v (first c))
	  (d (second c)))
      (if (zerop d) (nesl-error "Divide by zero.") 
	(typecase v
		  (integer (truncate (/ v d)))
		  (t (/ v d))))))

(defun prim-rem (c) 
    (let ((v (first c))
	  (d (second c)))
      (if (zerop d) (nesl-error "Divide by zero in REM.") (rem v d))))
    
(defun prim-mod (c) 
    (let ((v (first c))
	  (d (second c)))
      (if (zerop d) (nesl-error "Divide by zero in MOD.") (mod v d))))

;; does not handle lshift with -ve b
(defun prim-lshift (c) 
  (let ((a (first c))
	(b (second c)))
    (ash a b)))

(defun prim-rshift (c) 
  (let ((a (first c))
	(b (second c)))
    (ash a (- b))))

(defun prim-sqrt (v)
  (if (minusp v) (nesl-error "Srqt of negative number.") (sqrt v)))

(defun prim-ln (v) 
  (if (minusp v) (nesl-error "Ln of negative number.") (log v)))

(defun prim-log (c)
  (let ((v (first c))
	(b (second c)))
    (if (minusp v) (nesl-error "Log of negative number.") (log v b))))

(defun prim-exp (v) (exp v))

(defun prim-expt (c) 
    (let ((v (first c))
	  (p (second c)))
      (expt v p)))

(defun prim-one (a) 
    (typecase a
	      (integer 1)
	      (t 1.0d0)))

; need typecheck
(defun prim-zero (a) 
    (typecase a
	      (integer 0)
	      (t 0.0)))

(defun prim-^ (c)
  (let ((a (first c))
	(p (second c)))
    (cond ((zerop p) (prim-one a))
	  ((minusp p) (nesl-error "^ given a negative exponent"))
	  (t 
	   (do ((res a  (* res res))
		(pow p (truncate (/ pow 2)))
		(extra 1 (if (oddp pow) (* extra res)  extra)))
	       ((eql pow 1) (* res extra)))))))


(defun prim-btoi (a) (if a 1 0))

(defun prim-float (v) (coerce v 'double-float))

(defun prim-isqrt (v) 
  (if (minusp v) (nesl-error "Isqrt of negative number.") 
    (isqrt v)))

(defun prim-rand (v) (if (plusp v) (random v) 
		(nesl-error "Rand given non-positive argument")))

(defun prim-digitp (c) (and (char>= c #\0) (char<= c  #\9)))

(defun prim-start-timer (foo) 
  (declare (ignore foo))
  (get-internal-run-time))

(defun prim-stop-timer (start) 
  (coerce (/ (- (get-internal-run-time) start) 
	     internal-time-units-per-second)
	  'double-float))

(defun prim-zip-over (c) 
  (let ((a (first c))
	(b (second c)))
    (let ((len1 (nesl-seq-len a))
	  (len2 (nesl-seq-len b)))
      (if (not (eql len1 len2)) 
	  (nesl-runtime-error "Length mismatch in an apply-to-each.")
	(let ((res (make-array len1))
	      (type (list 'vector 
			  (list 'pair 
				(second (nesl-seq-type a))
				(second (nesl-seq-type b))))))
	  (do ((av (nesl-seq-value a))
	       (bv (nesl-seq-value b))
	       (i 0 (+ i 1)))
	      ((= i len1) t)
	      (setf (svref res i) (list (svref av i) (svref bv i))))
	  (make-nesl-seq :len len1 :value res :type type))))))

(defun prim-partition (c)
  (let ((v (first c))
	(counts (second c)))
  (let* ((res-len (nesl-seq-len counts))
	 (type (list 'vector (nesl-seq-type v)))
	 (counts (nesl-seq-value counts))
	 (sum-counts (reduce '+ counts)))
    (if (not (eql sum-counts (nesl-seq-len v)))
	(nesl-runtime-error "Length mismatch in function PARTITION.")
      (let ((values (nesl-seq-value v))
	    (res (make-array res-len)))
	(do ((start 0 (+ start (svref counts i)))
	      (i 0 (+ i 1)))
	     ((= i res-len) t)
	     (setf (svref res i) 
		   (make-nesl-seq 
		    :len (svref counts i) 
		    :value (subseq values start (+ start (svref counts i)))
		    :type (second type))))
	(make-nesl-seq :len res-len :value res :type type)))))  )

(defun prim-flatten (v) 
  (let* ((values (nesl-seq-value v))
	 (type (second (nesl-seq-type v)))
	 (len (nesl-seq-len v))
	 (res-len (do ((c 0 (+ c (nesl-seq-len (svref values i))))
		       (i 0 (+ i 1)))
		      ((= i len) c)))
	 (res (make-array res-len)))
    (do* ((start 0 (+ start (nesl-seq-len (svref values i))))
	  (i 0 (+ i 1)))
	 ((= i len) t)
	 (setf (subseq res start (+ start (nesl-seq-len (svref values i))))
	       (nesl-seq-value (svref values i))))
    (make-nesl-seq :len res-len :value res :type type)))

(defun prim-length (v) (nesl-seq-len v))

(defun prim-dist (c) 
  (let ((a (first c))
	(l (second c)))
    (let ((type (list 'vector (get-nesl-type a))))
      (make-nesl-seq :len l 
		     :value (make-array l :initial-element a)
		     :type type))))

(defun prim-seq_dist (c) 
  (let ((a (first c))
	(l (second c)))
    (let ((type (list 'vector (get-nesl-type a))))
      (make-nesl-seq :len l 
		     :value (make-array l :initial-element a)
		     :type type))))


(defun prim-dist-l (c) 
    (let ((v (first c))
	(w (second c)))
      (let ((len (nesl-seq-len w))
	    (type (list 'vector (get-nesl-type v))))
	(make-nesl-seq :len len 
		       :value (make-array len :initial-element v)
		       :type type))))



(defun prim-rep (c)
  (let ((d (first c))
	(rest (second c)))
    (let* ((len (nesl-seq-len d))
	   (pos (second rest))
	   (type (nesl-seq-type d))
	   (result (copy-seq (nesl-seq-value d))))
      (if (or (>= pos len) (< pos 0)) 
	  (nesl-runtime-error "Out of bounds index given to REP")
	(prog2
	    (setf (svref result (second rest)) (first rest))
	    (make-nesl-seq :len len :type type :value result))))))


(defun prim-elt (c) 
  (let ((a (first c))
	(i (second c)))
  (if (or (< i 0) (>= i (length (nesl-seq-value a)))) 
      (nesl-runtime-error "Seq reference a[i] out of bounds")
    (svref (nesl-seq-value a) i))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCANS
;;;;;;;;;;;;;;;;;;;;;;;;;
;; They all need type for identity elt

(defun guess-seq-type (seq)
  (if (= (nesl-seq-len seq) 0) (second (nesl-seq-type seq))
    (typecase (svref (nesl-seq-value seq) 0)
	      (integer 'int)
	      (character 'char)
	      (float 'float)
	      (t 'bool))))

(defmacro nesl-scan (func ident)
  `(do* ((len (nesl-seq-len seq))
	 (val (nesl-seq-value seq))
	 (type (guess-seq-type seq))
	 (res (make-array len))
	 (sum ,ident (,func sum (svref val i)))
	 (i 0 (+ i 1)))
	((= i len)      
	 (make-nesl-seq :len len :value res :type (list 'vector type)))
	(setf (svref res i) sum)))

(defun prim-plus_scan (seq) 
  (nesl-scan + (if (eql type 'float) 0.0d0 0)))

(defun prim-mult_scan (seq) 
  (nesl-scan * (if (eql type 'float) 1.0d0 1)))

(defun prim-max_scan (seq) 
  (if (eql (guess-seq-type seq) 'char)
      (nesl-scan max-char (code-char 0))
    (nesl-scan max (if (eql type 'int) *min_int* *min_float*))))

(defun prim-min_scan (seq) 
  (if (eql (guess-seq-type seq) 'char)
      (nesl-scan min-char (code-char 255))
    (nesl-scan min (if (eql type 'int) *max_int* *max_float*))))

(defun prim-or_scan (seq) 
  (if (eql (guess-seq-type seq) 'int)
      (nesl-scan logior 0)
    (nesl-scan nesl-or nil)))

(defun prim-and_scan (seq) 
  (if (eql (guess-seq-type seq) 'int)
      (nesl-scan logand -1)
    (nesl-scan nesl-and t)))

(defun prim-xor_scan (seq) 
  (if (eql (guess-seq-type seq) 'int)
      (nesl-scan logxor 0)
    (nesl-scan nesl-xor nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; REDUCES
;;;;;;;;;;;;;;;;;;;;;;;;;
;; all bool ops  need a type-check (bool/int)

(defmacro nesl-reduce (func ident)
  `(do* ((len (nesl-seq-len seq))
	 (val (nesl-seq-value seq))
	 (type (second (nesl-seq-type seq)))
	 (sum (if (= len 0) ,ident (svref val 0))
	      (,func sum (svref val i)))
	 (i 1 (+ i 1)))
	((>= i len) sum)))

(defun prim-sum (seq) 
  (nesl-reduce + (if (eql type 'int) 0 0.0d0)))

(defun prim-product (seq) 
  (nesl-reduce * (if (eql type 'int) 1 1.0d0)))

(defun prim-max_val (seq) 
  (if (eql (guess-seq-type seq) 'char)
      (nesl-reduce max-char (code-char 0))
    (nesl-reduce max (if (eql type 'int) *min_int* *min_float*))))

(defun prim-min_val (seq) 
  (if (eql (guess-seq-type seq) 'char)
      (nesl-reduce min-char (code-char 255))
    (nesl-reduce min (if (eql type 'int) *max_int* *max_float*))))

(defun prim-any (seq) 
  (if (eql (guess-seq-type seq) 'int)
      (nesl-reduce logior 0)
    (nesl-reduce nesl-or nil)))

(defun prim-all (seq) 
  (if (eql (guess-seq-type seq) 'int)
      (nesl-reduce logand -1)
    (nesl-reduce nesl-and t)))

(defun prim-parity (seq) 
  (if (eql (guess-seq-type seq) 'int)
      (nesl-reduce logxor 0)
    (nesl-reduce nesl-xor nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; GET ->
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prim--> (c) 
  (let ((v (first c))
	(i (second c)))
    (let* ((len (nesl-seq-len v))
	   (type (nesl-seq-type v))
	   (values (nesl-seq-value v))
	   (indices (nesl-seq-value i))
	   (res-len (nesl-seq-len i))
	   (res (make-array res-len)))
      (do ((i 0 (+ i 1)))
	  ((= i res-len) t)
	  (let ((idx (svref indices i)))
	    (when (or (>= idx len) (< idx 0))
		  (nesl-runtime-error 
		   "Index out of bounds for function ->."))
	  (setf (svref res i) (svref values idx))))
      (make-nesl-seq :len res-len :value res :type type))))

(defun prim-nocheck-> (c)   
  (prim--> c))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUT
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prim-<-  (c)
  (let ((d (first c))
	(i-v (second c)))
    (let* ((i-v-val (nesl-seq-value i-v))
	   (i-v-len (nesl-seq-len i-v))
	   (res-len (nesl-seq-len d))
	   (res-type (nesl-seq-type d))
	   (res (copy-seq (nesl-seq-value d))))
      (do ((i 0 (+ i 1)))
	  ((= i i-v-len) t)
	  (let ((idx (first (svref i-v-val i)))
		(val (second (svref i-v-val i))))
	    (if (or (< idx 0) (> idx res-len))
		(nesl-runtime-error 
		 "Index out of bounds for function <-."))
	    (setf (svref res idx) val)))
      (make-nesl-seq :len res-len :value res :type res-type))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDENTITY FOR ALL TYPES
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prim-identity-scalar (a) (typecase a
			(int   0)
			(float 0.0)
			(char  #\ )
			(bool  nil)))

(defun prim-identity (a)  (labels 
	       ((ident (a) 
		       (typecase a
				 (integer   0)
				 (float 0.0)
				 (character  #\ )
				 (cons (list (ident (first a)) 
					     (ident (second a))))
				 (nesl-seq (make-empty-vect (nesl-seq-type a)))
				 (datatype (make-datatype 
					    :name (datatype-name a)
					    :value (ident (datatype-value a))))
				 (t nil))))
	       (ident a)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACK
;;;;;;;;;;;;;;;;;;;;;;;;;


(defun prim-pack (v)
  (let* ((type (second (second (nesl-seq-type v))))
	 (v-f (nesl-seq-value v))
	 (v-f-len (nesl-seq-len v))
	 (res-len (do ((c 0 (if (second (svref v-f i)) (+ c 1) c))
		       (i 0 (+ i 1)))
		      ((= i v-f-len) c)))
	 (res (make-array res-len)))
    (do ((c 0 (if (second (svref v-f i)) (+ c 1) c))
	 (i 0 (+ i 1)))
	((= i v-f-len) t)
	(when (second (svref v-f i))
	  (setf (svref res c) (first (svref v-f i)))))
    (make-nesl-seq :len res-len :value res :type (list 'vector type))))

(defun internal-eql (a b) 
  (typecase a
	    (cons (and (internal-eql (first a) (first b)) 
		       (internal-eql (second a) (second b))))
	    (nesl-seq (and (eql (nesl-seq-len a)
				(nesl-seq-len b))
			   (let ((vala (nesl-seq-value a))
				 (valb (nesl-seq-value b))
				 (len (nesl-seq-len a)))
			     (do ((fl t (and fl (internal-eql 
						 (aref vala i)
						 (aref valb i))))
				  (i 0 (+ i 1)))
				 ((= i len) fl)))))
	    (datatype (and (eql (datatype-name a) (datatype-name b))
			   (internal-eql (datatype-value a) (datatype-value b))))
	    (t (eql a b))))

(defun prim-eql (c)
  (let ((a (first c))
	(b (second c)))
    (internal-eql a b)))

(defun prim-++ (c)
  (let ((v1 (first c))
	(v2 (second c)))
    (let* ((len1 (nesl-seq-len v1))
	   (len2 (nesl-seq-len v2))
	   (val1 (nesl-seq-value v1))
	   (val2 (nesl-seq-value v2))
	   (type (nesl-seq-type v1))
	   (res-len (+ len1 len2))
	   (res (make-array res-len)))
      (setf (subseq res 0 len1) val1)
      (setf (subseq res len1 res-len) val2)
      (make-nesl-seq :len res-len :value res :type type))))
    
(defun hash-float (a)
  (if (= a 0.0) 0
    (let* ((a (abs a))
	   (lg (truncate (log a 2.0)))
	   (sig (truncate (/ a (expt 2.0 (float (- lg 30)))))))
      (- sig lg))))

(defun prim-hashinternal (a) 
  (typecase 
   a
   (float (hash-float a))
   (integer a)
   (character  (char-code a))
   (cons (+ (prim-hashinternal (first a)) 
	    (* 991892311 (prim-hashinternal 
			  (second a)))))
   (nesl-seq 
    (do ((sum 0 (+ sum 
		   (prim-hashinternal (svref (nesl-seq-value a) i))))
	 (i 0 (+ i 1)))
	((= i (nesl-seq-len a)) sum)))
   (t (if a 1 0))))

(defun prim-flag_merge (c)
  (let ((flags (first c))
	(rest (second c)))
    (let* 
	((flag-len (nesl-seq-len flags))
	 (flag-vals (nesl-seq-value flags))
	 (res (make-array flag-len))
	 (type (nesl-seq-type (first rest)))
	 (v1 (nesl-seq-value (first rest)))
	 (v2 (nesl-seq-value (second rest))))
      (do ((c1 0 (if (svref flag-vals i) c1 (+ c1 1)))
	   (i 0 (+ i 1)))
	  ((= i flag-len) t)
	  (setf (svref res i)
		(if (svref flag-vals i) (svref v2 (- i c1))
		  (svref v1 c1))))
      (make-nesl-seq :len flag-len :value res :type type))))

(defun prim-prim-member (c)	  
  (let ((a (first c))
	(v (second c)))
    (member a (coerce (nesl-seq-value v) 'list))))

(defun prim-sort (v)
    (let ((len (nesl-seq-len v))
	  (type (nesl-seq-type v))
	  (value (sort (copy-seq (nesl-seq-value v)) '<)))
      (make-nesl-seq :len len :value value :type type)))


(defun prim-prim-inv-rank (v-i)
    (let* ((len (nesl-seq-len v-i))
	  (type (nesl-seq-type v-i))
	  (key 'first)
	  (value (stable-sort (copy-seq (nesl-seq-value v-i)) '< :key key)))
      (make-nesl-seq :len len :value value :type type)))
	  


(defun prim-prim-inv-rank-down (v-i)
    (let* ((len (nesl-seq-len v-i))
	  (type (nesl-seq-type v-i))
	  (key 'first)
	  (value (stable-sort (copy-seq (nesl-seq-value v-i)) '> :key key)))
      (make-nesl-seq :len len :value value :type type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IO Prims
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prim-prim-print-char (a)
  (write-char a) (finish-output) t)


(defun prim-print_string (v)
  (let ((str (coerce (nesl-seq-value v) 'string)))
    (write-string str)
    (finish-output)
    t))

(defun prim-prim-open-file (c)
  (let ((filename (first c))
	(mode (second c)))
    (let* ((filename (coerce (nesl-seq-value filename) 'string)))
      (multiple-value-bind (ret-val)
	   (nesl-ignore-errors 
	    (cond ((eql mode 1) 
		   (open filename :direction :input :if-does-not-exist nil))
		  ((eql mode 2)
		   (open filename :direction :output :if-exists :supersede))
		  ((eql mode 3)
		   (open filename :direction :output :if-exists :append))
		  (t nil)))
	   (assign-new-stream ret-val (if ret-val t))))))

(defun prim-prim-close-file (str)
  (let ((file-str (free-stream str)))
    (if file-str (close file-str))
    t))

(defun prim-prim-read-char (str)
  (if (not (input-stream-p str)) (list #\  nil)
    (let ((char (read-char str nil nil)))
      (if char (list char t t) (list #\  t)))))

(defun prim-read-delim (c)
  (let ((len (first c))
	(rest (second c)))
    (read-delim len (car rest) 
		    (char-list (cadr rest)))))

(defun prim-read-delim-no-hang (c)
  (let ((len (first c))
	(rest (second c)))
    (read-delim len (car rest) 
		    (char-list (cadr rest)) t)))

(defun prim-prim-sleep (len)
  (sleep  (/ len *sleep-factor*))
  t)

(defun prim-prim-lookup (str)
  (let ((str (lookup-stream str)))
    (if str (list str t) (list str nil))))

(defun prim-prim-read-lookup (str)
  (let ((str (lookup-stream str)))
    (if (and str (input-stream-p str)) (list str t) (list nil nil))))

(defun prim-prim-write-char (c)
  (let ((ch (first c))
	(str (second c)))
    (if (output-stream-p str) 
	(progn (write-char ch str) (finish-output str) t)
      nil)))

(defun prim-prim-write-string (c)
  (let ((v (first c))
	(str (second c)))
    (if (output-stream-p str) 
	(progn t
	       (format str "~a" (coerce (nesl-seq-value v) 'string))
	       (force-output str)
	       (finish-output str)
	       t)
      nil)))

(defun prim-prim-read-int-seq (str)
  (let* ((seq-flag1 (prim-read-object str))
	 (int-list (first seq-flag1))
	 (flag1 (second seq-flag1)))
    (multiple-value-bind (int-list flag2) 
      (nesl-ignore-errors (mapcar #'(lambda (x) (coerce x 'int))
			     int-list))
      (let ((seq (make-nesl-seq :len (length int-list) 
				:value (coerce int-list 'vector)
				:type '(vector int))))
	(list seq (and flag1 (not flag2)))))))

(defun prim-prim-read-float-seq (str)
  (let* ((seq-flag1 (prim-read-object str))
	 (float-list (first seq-flag1))
	 (flag1 (second seq-flag1)))
    (multiple-value-bind (float-list flag2) 
      (nesl-ignore-errors (mapcar #'(lambda (x) (coerce x 'float))
			     float-list))
      (let ((seq (make-nesl-seq :len (length float-list) 
				:value (coerce float-list 'vector)
				:type '(vector float))))
	(list seq (and flag1 (not flag2)))))))

(defun prim-prim-read-nesl-object (c)
  (let ((obj (first c))
	(str (second c)))
    (let* ((seq-flag1 (prim-read-object str))
	   (read-obj (first seq-flag1))
	   (flag1 (second seq-flag1)))
      (if (not flag1) (list obj flag1)
	(multiple-value-bind (type)
		      (get-nesl-type read-obj)
		      (let ((type0 (get-nesl-type obj)))
			;;  (format t "type = ~a, type0 = ~a" type type0)
			(if (or (equal type type0)
				(null type) (null type0))
			    (list read-obj t)
			  (list obj nil))))))))


(defun prim-prim-write-nesl-object (c)
  (let ((obj (first c))
	(str (second c)))
    (if (output-stream-p str)
	(progn 
	  (write obj :stream str)
	  (finish-output str)
	  t)
      nil)))

(defun prim-base_typecase (a)
  (typecase a
	    (integer 1)
	    (float 2)
	    (character 3)
	    (boolean 4)
	    (func 5)
	    (t 4)))

(defun prim-poly-typecase (a)
  (typecase a
	    (nesl-seq 1)
	    (cons 4)
	    (datatype 3)
	    (t 2)))

(defun prim-datatype-val (a)
  (datatype-value a))

(defun prim-string (v &optional v2)
;  (let* ((v (if v2 (list v v2) v))
  (declare (ignore v2))
  (let* (
	 (type (if (nesl-seq-p v) (nesl-seq-type v) nil))
	 (string (cnesl-print-data v t type)))
    (char-vect string)))

(defun prim-spawn (command &optional others)
  #-allegro
  (let ((null-str (nullstream)))
    (list (list null-str (list null-str null-str)) 
	(list nil 
	      (char-vect "This function is currently not supported in your version of Lisp."))))
  #+allegro
  (let* ((in-stream (car others))
	 (out-stream (caadr others))
	 (err-stream (cadadr others))
	 (in-str (if (is-nullstr in-stream) :stream (lookup-stream in-stream)))
	 (o-str (if(is-nullstr out-stream) :stream (lookup-stream out-stream)))
	 (e-str (if(is-nullstr err-stream) :stream (lookup-stream err-stream)))
	 (comm (coerce (nesl-seq-value command) 'string))
	 (null-str (nullstream)))
    (if (not (and in-str o-str e-str))
	(list (list null-str (list null-str null-str)) 
	      (list nil (char-vect "Error in stream lookup.")))
      (if (not (and (or (eql in-str :stream)(input-stream-p in-str))
		    (or (eql o-str :stream)(output-stream-p o-str))
		    (or (eql e-str :stream)(output-stream-p e-str))))
	  (list (list null-str (list null-str null-str)) 
		(list nil (char-vect "Incorrect directions for streams.")))
	(multiple-value-bind 
	    (str-a str-b)
	  (system::run-shell-command comm :input in-str :output o-str 
				     :error-output e-str :wait nil)
	  (let ((in-stream (if (and (is-nullstr in-stream) (new-stream-check))
			       (create-str (car (assign-new-stream str-a t)))
			     in-stream))
		(out-stream (if (and (is-nullstr out-stream)(new-stream-check))
				(create-str (car (assign-new-stream str-a t)))
			      out-stream))
		(err-stream (if (and (is-nullstr err-stream) (new-stream-check))
				(create-str (car (assign-new-stream str-b t)))
			      err-stream)))
	    (if (or (is-nullstr in-stream) 
		    (is-nullstr out-stream) 
		    (is-nullstr err-stream))
		(list (list null-str (list null-str null-str)) 
		      (list nil (char-vect "Could not create new streams.")))
	      (progn (flush-str out-stream) (flush-str err-stream)
		     (list (list in-stream (list out-stream err-stream))
			   (list t (char-vect "")))))))))))

(defun prim-clean-up (ignore)
  (declare (ignore ignore))
  #+allegro (system:os-wait)
  #-allegro nil)

(defun prim-get_environment_variable (var)
  #+allegro (char-vect (sys:getenv (coerce (nesl-seq-value var) 'string)))
  #-allegro (nesl-runtime-error "Not implemented for this version of lisp"))

