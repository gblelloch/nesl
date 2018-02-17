(in-package :nesl-lisp)

(defparameter *header*
"
% This file generated automatically from the function specifications.
\\section{List of Functions}
\\applabel{functions}

\\newcommand{\\rar}{$\\rightarrow$}
\\newcommand{\\farg}[1]{{\\tt #1}}
\\newcommand{\\tdefop}[2]{
  \\vspace{.15in} \\noindent 
  {\\tt #1} \\hfill {\\em \\{#2\\} }\\\\[.05in] }
\\newcommand{\\ttdefop}[2]{
  \\vspace{.15in} \\noindent 
  {\\tt #1}\\\\ 
  \\mbox{} \\hfill {\\em \\{#2\\} }\\\\[.05in] }

This section lists the functions and constants available in \\nesl.
Each function is listed in the following way:

\\vspace{.05in}
\\tdefop{{\\em function interface}}{type}
Definition of function.
\\vspace{.2in}

\\noindent The hierarchy of the type classes is shown in \\figref{typeclasses}.
")

(defparameter *tlist* 
  '((a0 "$a_0$") (a1 "$a_1$") (a2 "$a_2$") (a3 "$a_3$")
    (a4 "$a_4$") (a5 "$a_5$") (a6 "$a_6$") (a7 "$a_7$")
    (b0 "$b_0$") (b1 "$b_1$") (b2 "$b_2$") (b3 "$b_3$")
    (b4 "$b_4$") (b5 "$b_5$") (b6 "$b_6$") (b7 "$b_7$")
    (inf "$\\infty$") (minf "$-\\infty$")
    (t "T") (f "F")))

(defparameter alpha "a")

(defun gen-latex-element (element stream)
  (cond ((atom element) 
	 (format stream "~a" 	  
		 (if (and (constantp element) (not (eql element t)))
		     element
		   (second (assoc element *tlist*)))))
	((listp element)
	 (format stream "[")
	 (gen-latex-element (car element) stream)
	 (dolist (element (cdr element))
	   (format stream ", ")
	   (gen-latex-element element stream))
	 (format stream "]"))))
      
(defun gen-latex-sequence (name elements stream)
  (format stream "  \\farg{~(~a~)} & = & " name)
  (gen-latex-element elements stream)
  (format stream "\\\\~%"))

(defun gen-latex-example (args interface example stream)
  (format stream "{\\tt \\tabcolsep 4pt \\defexample{lcl}{~%")
  (do ((arg-values (cddr example) (cdr arg-values))
       (arg-names (flatten-pair (second args)) (cdr arg-names)))
      ((null arg-names))
      (gen-latex-sequence (first arg-names) (first arg-values) stream))
  (format stream "  [.07in]~%")
  (gen-latex-sequence interface (first example) stream)
  (format stream "}}~%"))

(defun list-replace-specials (list)
  (if list 
      (let ((rest (list-replace-specials (cdr list))))
	(cond ((member (car list) '(#\_ #\#))
	       (cons #\\ (cons (car list) rest)))
	      (t (cons (car list) rest))))
    nil))

(defun replace-specials (string)
  (coerce (list-replace-specials (coerce string 'list)) 'string))

(defun latex-string (a)
  (replace-specials (string-downcase (string a))))

(defun list-replace-arrows (list)
  (if list 
      (cond ((eql (car list) #\_)
	     (cons #\\ (cons #\_ (list-replace-arrows (cdr list)))))
	    ((and (eql (car list) #\-) (eql (second list) #\>))
	     (append `(#\\ #\r #\a #\r) (list-replace-arrows(cddr list))))
	    (t (cons (car list) (list-replace-arrows (cdr list)))))
    nil))

(defun replace-arrows (string)
  (coerce (list-replace-arrows (coerce string 'list)) 'string))

(defun latex-type (type)
  (replace-arrows (pprint-oneline-string (cnesl-full-type type))))

(defun latex-header (interface type stream)
  (let* ((type (latex-type type))
	 (longtype? (> (+ (length type) (length interface)) 90)))
    (format stream "~(\\~a{~a}{~a}~)~%"
		(if longtype? "ttdefop" "tdefop")
		interface type)))

(defun gen-latex-description (function stream definitions)
  (cond ((stringp function)
	 (write-string (replace-specials function) stream)
	 (format stream "~%~%"))	
	((symbolp function)
	 (let ((binding (get-binding-definition function definitions)))
	   (when (not binding)
	     (error "While generating documentation, no such function ~a."
		    function))
	   (let* ((func (binding-original-code binding))
		  (keys (func-other func))
		  (args (cons (func-name func) (func-arguments func)))
		  (types (func-type func))
		  (interface (latex-string 
			      (pprint-oneline-string 
			       (cnesl-exp (if (is-function-type? types)
					      args
					    (car args)) 1))))
		  (documentation (get-keyword :documentation keys nil))
		  (example (get-keyword :example keys nil)))
	     (latex-header interface types stream)
	     (format stream "\\index{\\tt ~a}~%" (latex-string (car args)))
	     (when documentation
	       (format stream "~a~%" (replace-specials documentation)))
	     (when example
	       (format stream "For example:~%~%")
	       (gen-latex-example args interface example stream))
	     (format stream "~%"))))))

(defun write-all (doclist file definitions)
  (with-open-file (ofile file 
			 :direction :output
			 :if-exists :supersede)
    (format ofile "~a~%" *header*)
    (dolist (op doclist)
      (gen-latex-description op ofile definitions))))

(defun make-doc ()
  (declare (special *doclist* *definitions*))
  (write-all *doclist* 
	     "/afs/cs/project/scandal/papers/nesl/manual.3.1/funlist.tex"
	     *definitions*))

(defun apropos-nesl (args definitions)
  (when (not (= (length args) 1))
    (nesl-error "Apropos should be in the form: \"apropos <name>\"."))
  (let* ((name (first args))
	 (funcs (nesl-apropos-all (string name) 
				  (definition-table-bindings definitions))))
    (dolist (func funcs)
      (format t "~%~(~a~) : ~a" 
	      (func-name func)
	      (pprint-oneline-string (cnesl-full-type (func-type func)))))))

(defun nesl-apropos-all (name bindings)
  (if bindings
      (let* ((func (binding-original-code (cdar bindings)))
	     (docstring (get-keyword :documentation (func-other func) nil))
	     (fname (string (caar bindings))))
	(if (and (not (or (find #\- fname) (search "PRIM_" fname)))
		 (or (search name fname)
		     (and docstring 
			  (search name docstring :test #'char-equal))))
	    (cons func (nesl-apropos-all name (cdr bindings)))
	  (nesl-apropos-all name (cdr bindings))))
    nil))

(defparameter *doclist* '(

  "\\subsection{Scalar Functions} \\applabel{scalar-ops}"
  
  "\\subsubsection*{Logical Functions}
  All the logical functions work on either integers or booleans.
  In the case of integers, they work bitwise over
  the bit representation of the integer."
  
  not or and xor nor nand

  "\\subsubsection*{Comparison Functions}
  All comparison functions work on any type of the {\\em ordinal} type-class, 
  which includes integers, floats and characters."

  == /= < > <= >= 

  "\\subsubsection*{Predicates}"

  plusp minusp zerop oddp evenp

  "\\subsubsection*{Arithmetic Functions}"
  
  + - negate abs diff max min * / rem lshift rshift sqrt isqrt 
  ln log exp expt sin cos tan asin acos atan sinh cosh tanh

  "\\subsubsection*{Conversion Functions}"

  btoi code_char char_code float ceil floor trunc round

  "\\subsubsection*{Constants}"

  pi max_int min_int

  "\\subsubsection*{Other Scalar Functions}"

  rand rand_seed

  "\\subsection{Sequence Functions} \\applabel{sequence-ops}"

  "\\subsubsection*{Simple Sequence Functions}"

   |#| dist elt rep zip unzip

  "\\subsubsection*{Scans and Reduces}"

  plus_scan max_scan min_scan or_scan and_scan
  iseq index
  sum max_val min_val any all
  count max_index min_index

  "\\subsubsection*{Sequence Reordering Functions}"

  -> read permute <- write rotate reverse

  "\\subsubsection*{Simple Sequence Manipulation}"

  pack ++ subseq drop take odd_elts even_elts interleave
  length_from_flags
  ;; replace_subseq

  "\\subsubsection*{Nesting Sequences}
  The two functions {\\tt partition} and
  {\\tt flatten} are the primitives for moving between levels of
  nesting.  All other functions for moving between levels of nesting
  can be built out of these.  The functions {\\tt split} and {\\tt bottop}
  are often useful for divide-and-conquer routines."

  partition flatten split bottop head_rest rest_tail

  "\\subsubsection*{Other Sequence Functions}
These are more complex sequence functions.  The depth complexities
of these functions are not necessarily $O(1)$."

  sort rank collect int_collect kth_smallest find
  search_for_subseqs remove_duplicates mark_duplicates
  union intersection name
  transpose

  "\\subsection{Functions on Any Type}"

  eql hash select identity

  "\\subsection{Functions for Manipulating Strings}"

  @ exp_string |\|\||
  linify wordify 
  lowercase uppercase string_eql 
  parse_int parse_float

  "\\subsection{Functions with Side Effects}

The functions in this section are not purely functional.  Unless
otherwise noted, none of them can be called in parallel---they cannot
be called within an apply-to-each construct.  The routines in this
section are not part of the core language, they are meant for
debugging, I/O, timing and display.  Because these functions are new
it is reasonably likely that the interface of some of these functions
will change in future versions.  The user should check the most recent
documentation."

  "\\subsubsection*{Input and Output Routines}
Of the functions listed in this section, only {\\tt print_char},
{\\tt print_string},{\\tt write_char},
{\\tt write_string}, and {\\tt write_check} can be called in
parallel."

  print_char print_string
  write_object_to_file  write_string_to_file
  append_string_to_file
  read_object_from_file read_string_from_file
  read_int_seq_from_file read_float_seq_from_file
  open_in_file open_out_file close_file 
  write_char write_string
  read_char read_string read_line read_word 
  open_check write_check read_check close_check
  nullstr stdin stdout stderr

  "\\subsubsection*{Plotting Functions}
The functions in this section can be used for plotting data on an
{\\em Xwindow} display.
The basic idea of these functions is that you create a window with
the {\\tt w\_make\_window} command and then can add various features
to the window.  The most important features are boxes and buttons.
A scale box can be used to create a virtual coordinate system on the
window on which points, lines, rectangles and polygons can be drawn.
A text box can be used to create a box in which text can be written.
A button can be used along with the input functions to get information
back from the window.
In all the functions in this section colors are specified
by one of 
{\\tt w\_black},
{\\tt w\_white},
{\\tt w\_red},
{\\tt w\_blue},
{\\tt w\_green},
{\\tt w\_cyan},
{\\tt w\_yellow},
{\\tt w\_magenta},
{\\tt w\_pink},
{\\tt w\_light_green},
{\\tt w\_light_blue},
{\\tt w\_purple},
{\\tt w\_gray},
{\\tt w\_dark_gray},
or {\\tt w\_orange}.
"
  display

  w_make_window w_kill_window
  w_add_box w_add_text_box w_add_button w_add_button_stack
  w_add_text

  w_get_named_box w_reset_box_size w_clear_box
  w_bounds_from_box w_box_scale w_bounding_box

  w_draw_point w_draw_big_point w_draw_points 
  w_draw_segments w_draw_string w_draw_rectangle 
  w_shade_rectangle w_shade_polygon

  w_write_text_centered w_write_text_left w_write_paragraph
  w_get_input w_get_input_noblock w_get_button_input w_get_zoom_box

  "\\subsubsection*{Shell Commands}
The functions in this section can be used to execute shell commands
from within Nesl."

  shell_command get_environment_variable spawn

  "\\subsubsection*{Other Side Effecting Functions}"

  time
  
))
