;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NESL mode for editing, interpreting.
;;;
;;; NESL formatting by Tom Sheffler.
;;; Built on initial version by Guy Blelloch.
;;; Feb 1993
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HISTORY
;;;
;;; V 1.1: Tom Sheffler, Feb 1993
;;; 	Automatic function type annotation of function definition.
;;; 	Pretty print in a popup window. 
;;;
;;; V 1.0: Tom Sheffler, Feb 1993
;;; 	Basic NESL indentation code.
;;; 	Comment syntax.
;;;
;;; V 0.9: Guy Blelloch created, basic send-defun
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To speed things up and only autoload the code when its needed, do this:
;;; 1) Copy nesl-mode.el and nesl.el to your emacs-lisp source directory.
;;;    If you don't have one, you can create one with the following:
;;;     a) mkdir ~/emacs       [or wherever you want to put it]
;;;     b) add the following line (without the ;;;) to your .emacs file
;;;        (setq load-path (append (expand-file-name "~/emacs") load-path))
;;; 2) Comment out the line 10 lines below this one with a ;
;;; 3) From within GNU Emacs, byte-compile the files (M-x byte-compile,
;;;    ~/emacs/nesl-mode.el and M-x byte-compile, ~/emacs-nesl.el
;;; 4) Add the following four lines (without the ;;;) to your .emacs file:
;;;    (autoload 'run-nesl "nesl" "Major mode for running a NESL process." t)
;;;    (autoload 'nesl-mode "nesl-mode" "Major mode for editting NESL code." t)
;;;    (setq auto-mode-alist (append '(("\\.nesl$" . nesl-mode)) 
;;;                                   auto-mode-alist))

;;; Comment out the following line (put a ";" at its beginning) if auto-loading
(setq auto-mode-alist (append '(("\\.nesl$" . nesl-mode)) auto-mode-alist))

;;; Mode Variables
(defconst nesl-indent-level 2
  "*Indentation to be used inside of Nesl blocks or arrays")

(defconst nesl-tab-width 2
  "*Tab stop width for Nesl mode")

(defconst nesl-scroll-after-send-defun nil
  "If t, forces redisplay and scrolling of *nesl* buffer")

(defun nesl-make-tabs (stop)
  (and (< stop 132) (cons stop (nesl-make-tabs (+ stop nesl-tab-width)))))

(defconst nesl-tab-stop-list (nesl-make-tabs nesl-tab-width)
  "*Tab stop list for nesl mode")

(defvar nesl-tab-always-indent t
  "TAB keys means always re-indent current line rather than insert TAB.")

(defvar nesl-process "nesl"
  "String name of process that is inferior Nesl")

(defvar nesl-mode-hook nil
  "User-definable function hook called on entry to nesl-mode")

(defvar nesl-mode-map nil
  "Key map for NESL mode.")

(defvar nesl-mode-syntax-table nil
  "Syntax table in use in Nesl-mode buffers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if nesl-mode-map			; set once upon loading
    ()
  (setq nesl-mode-map (make-sparse-keymap))
  (define-key nesl-mode-map "\e\C-x"	'nesl-send-defun-and-maybe-scroll)
  (define-key nesl-mode-map "\e\C-a"	'beginning-of-nesl-function)
  (define-key nesl-mode-map "\e\C-e"	'end-of-nesl-function)
  (define-key nesl-mode-map "\t"   	'nesl-indent-command)
  (define-key nesl-mode-map "\C-cp" 	'nesl-pretty-print-defun)
  (define-key nesl-mode-map "\C-ct" 	'nesl-insert-function-type)
  )

(if nesl-mode-syntax-table		; set once upon loading
    ()
  (setq nesl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" nesl-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" nesl-mode-syntax-table)
  (modify-syntax-entry ?/ "." nesl-mode-syntax-table)
  (modify-syntax-entry ?* "." nesl-mode-syntax-table)
  (modify-syntax-entry ?+ "." nesl-mode-syntax-table)
  (modify-syntax-entry ?- "." nesl-mode-syntax-table)
  (modify-syntax-entry ?= "." nesl-mode-syntax-table)
  (modify-syntax-entry ?% "$" nesl-mode-syntax-table)
  (modify-syntax-entry ?< "." nesl-mode-syntax-table)
  (modify-syntax-entry ?> "." nesl-mode-syntax-table)
  (modify-syntax-entry ?& "." nesl-mode-syntax-table)
  (modify-syntax-entry ?| "." nesl-mode-syntax-table))

(defun nesl-mode-variables ()
  "Function that sets local NESL mode variables."
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-hook)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'tab-stop-list)
  (setq comment-start "% "
	comment-end " %"
	comment-start-skip "%<? *"	; Regexp to skip until comment body
	comment-column 40
    	comment-indent-hook 'nesl-comment-indent
	indent-line-function 'nesl-indent-line
	tab-stop-list nesl-tab-stop-list
	)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nesl-indent-command (&optional whole-exp)
  (interactive "P")
  "Indent current line as Nesl code, or in some cases insert a tab character.
If nesl-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (if whole-exp
      ;; If arg, always indent this line as NESL
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (nesl-indent-line))
	    beg end)
	(save-excursion
	  (if nesl-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "%<")))
    (if (and (not nesl-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (nesl-indent-line))))

(defun nesl-indent-line ()
  "Indent current line as Nesl code.
Return the amount the indentation changed by."
  (let ((indent (calculate-nesl-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-nesl-indent-within-comment)))
	  (t
	   (skip-chars-forward " \t")
	   ;; Special cases for alignment.
	   (cond ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (nesl-backward-to-start-of-if)
				 (current-indentation))))
		 ((and (looking-at "then\\b")
		       (not (looking-at "then\\s_")))
		  (setq indent (save-excursion
				 (nesl-backward-to-start-of-if)
				 (current-indentation))))
		 ((looking-at "in\\b")
		  (setq indent (save-excursion
				 (nesl-backward-to-start-of-let)
				 (current-indentation)))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))


(defun calculate-nesl-indent (&optional parse-start)
  "Return appropriate indentation for current line as C code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-nesl-function))
      (while (< (point) indent-point)
	;; TOM: move through at equal levels until we reach POINT
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0)))

      ;; TOM: innermost containing list: NIL if none
      (setq containing-sexp (car (cdr state)))

;;; TOM: for debugging/understanding!!
;;;      (save-excursion
;;;	(set-buffer "*scratch*")
;;;	(insert  (prin1-to-string state))
;;;	(insert (prin1-to-string containing-sexp))
;;;	(insert "\n")
;;;	)

      ;; TOM: if in comment, or in STRING just return nil.
      ;; TOM: comments aren't parsed right because % is given $ status
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     ;; TOM: inside COMMENT or STRING ??
	     (nth 4 state))
	    
	    ;; TOM: we're inside an expression.
	    (containing-sexp 
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))

	    ;; else, PUNT: just line it up with previous line
	    (t
	     (beginning-of-line)
	     (delete-horizontal-space)
	     ;; Check for LEFT-MARGIN constructs.
	     (if (or
		  (looking-at "%<<") ; Left margin comment?
		  (looking-at "function")
		  (looking-at "datatype")
		  (looking-at "\\$")
		  )
		 0			; no indentation
	       ;; ELSE, try to be more clever
	       (save-excursion
		 ;; Look at previous line.
		 (nesl-backward-to-noncomment 1)
		 (let ((retval (nesl-is-continuation-line)))
		   ;; if non-nil, then retval is either the keyword of
		   ;; previous line, or simply 't.
		   (if retval
		       (cond
			((equal retval '("let"))
			 (+ (current-indentation) nesl-indent-level))
			((equal retval '("in"))
			 (+ (current-indentation) nesl-indent-level))
			((equal retval '("if"))
			 (+ (current-indentation) nesl-indent-level))
			((equal retval '("then"))
			 (+ (current-indentation) nesl-indent-level))
			((equal retval '("else"))
			 (+ (current-indentation) nesl-indent-level))
			(t
			 (progn
			   ;; This line is a continuation of a stmt.
			   ;; Find beginning of this stmt and align to it.
			   (nesl-backward-to-start-of-continued-exp 1)
			   (current-indentation))))
		     ;; ELSE - prev line is NOT a continuation
		     ;; Align next line to this one
		     (current-indentation)
		     )))
	       ))))))
	       
(defun calculate-nesl-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end star-start)
    (save-excursion
      (save-excursion
	(set-buffer "*scratch*")
	(insert  "in INDENT-WITHIN-COMMENT")
	(insert "\n")
	)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (skip-chars-backward " \t\n")	; previous line
      (setq end (point))		; stop here
      (beginning-of-line)
      (skip-chars-forward " \t")
      (current-column))))

(defun nesl-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (beginning-of-line)
      (if (and (search-forward "%" opoint 'move)
	       (< lim (point)))
	  (forward-char -1)
	(setq stop t)))))

(defun nesl-backward-to-start-of-continued-exp (lim)
  (if (memq (preceding-char) '(?\) ?\] ?\}))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun nesl-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (or limit (setq limit (save-excursion (beginning-of-nesl-function)(point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (not (zerop if-level))
      (backward-sexp 1)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun nesl-backward-to-start-of-let (&optional limit)
  "Move to the start of the last ``unbalanced'' let."
  (or limit (setq limit (save-excursion (beginning-of-nesl-function) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (not (zerop if-level))
      (backward-sexp 1)
      (cond ((looking-at "in\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "let\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun nesl-mode ()
  "Major mode for editing NESL code."
  (interactive)
  (kill-all-local-variables)
  (use-local-map nesl-mode-map)
  (setq major-mode 'nesl-mode)
  (setq mode-name "Nesl")
  (set-syntax-table nesl-mode-syntax-table)
  (nesl-mode-variables)
  (run-hooks nesl-mode-hook))

;;; comments of the form "%<" stay where they are,
;;; single "%" comments go to the right margin.
(defun nesl-comment-indent ()
  (if (looking-at "%<")
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column)))
	 comment-column)))

;;; Are we on a continuation line?
;;; Return 't or 'nil, or if keyword, return it
(defun nesl-is-continuation-line ()
  (let* ((ch (preceding-char))
	 (ch-syntax (char-syntax ch)))
    (if (eq ch-syntax ?w)
	;; If prev char is a character, see if reserved word, return it
	(assoc (buffer-substring
		(progn (forward-word -1) (point))
		(progn (forward-word 1) (point)))
	       '(("let") ("in") ("if") ("then") ("else")))
      
      ;; We're ONLY on a continuation line if this line
      ;; is inside a paren expression.
      (save-excursion
	(beginning-of-line)
	(let ((indent-point (point))
	      (case-fold-search nil)
	      state
	      containing-sexp)
	  (beginning-of-nesl-function)
	  (while (< (point) indent-point)
	    ;; TOM: move through at equal levels until we reach POINT
	    (setq parse-start (point))
	    (setq state (parse-partial-sexp (point) indent-point 0)))
	  (setq containing-sexp (car (cdr state)))
	  containing-sexp)))))
    
(defun nesl-backward-to-start-of-continued-exp (lim)
;;  (if (memq (preceding-char) '( ?\) ?\] ))
;;      (forward-sexp -1))
  (while (nesl-is-continuation-line)
    (end-of-line 0))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun beginning-of-nesl-function (&optional arg)
  "Move backward to next beginning-of-function.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (and (re-search-backward "^function" nil 'move (or arg 1))
       (progn (beginning-of-line) t)))

(defun end-of-nesl-function (&optional arg)
  "Move backward to next beginning-of-defun.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (and (re-search-forward "\\$" nil 'move (or arg 1))
       (progn (end-of-line) t)))


(defun nesl-send-defun-and-maybe-scroll (ignore) 
  (interactive "P") 
  (nesl-send-defun nesl-scroll-after-send-defun))


(defun nesl-send-defun (display-flag)
  "Send the current defun to the Nesl process made by M-x run-nesl.
With argument, force redisplay and scrolling of the *nesl* buffer.
Variable `inferior-nesl-load-command' controls formatting of
the `load' form that is set to the Lisp process."
  (interactive "P")
  (or (get-process nesl-process)
      (error "No current lisp process"))
  (save-excursion
   (end-of-nesl-function)
   (let ((end (point))
	 (filename (format "/tmp/emlisp%d.nesl" 
			   (process-id (get-process nesl-process)))))
     (beginning-of-nesl-function)
     (write-region (point) end filename nil 'nomessage)
     (process-send-string
      nesl-process (format inferior-nesl-load-command filename)))
   (if display-flag
       (nesl-redisplay-process-window))
   ))


;;; After sending an expression to the NESL sub-process, move point to
;;; end of output, and display the process buffer window.
(defun nesl-redisplay-process-window ()
  (let* ((process (get-process nesl-process))
	 (buffer (process-buffer process))
	 (w (or (get-buffer-window buffer) (display-buffer buffer)))
	 (height (window-height w))
	 (end))
    (save-excursion
      (set-buffer buffer)
      (setq end (point-max))
      (while (progn
	       (accept-process-output process)
	       (goto-char (point-max))
	       (beginning-of-line)
	       (or (= (point-max) end)
		   (not (looking-at inferior-lisp-prompt)))))
      (setq end (point-max))
      (vertical-motion (- 4 height))
      (set-window-start w (point)))
    (set-window-point w end)))

;;; After sending an expression to the NESL sub-process, 
;;; return interpreter output as a string.
;;; Make sure it doesn't change the windows around.
(defun nesl-get-process-output ()
  (save-window-excursion
    (let* ((process (get-process nesl-process))
	   (buffer (process-buffer process))
	   (w (or (get-buffer-window buffer) (display-buffer buffer)))
	   (begin)
	   (end))
      (set-buffer buffer)
      (setq begin (point-max))
      (setq end (point-max))
      (while (progn
	       (accept-process-output process)
	       (goto-char (point-max))
	       (beginning-of-line)
	       (or (= (point-max) end)
		   (not (looking-at inferior-lisp-prompt)))))
      (setq end (point))
      (set-window-point w (point-max))
      (buffer-substring begin end))))


;;; Display the string given in the NESL pop-up window!
(defun nesl-popup-window (s)
  (let* (
	 (cbuf (current-buffer))
	 (buffer (get-buffer-create "*nesl-popup*"))
	 (w (or (get-buffer-window buffer) (display-buffer buffer)))
;;	 (height (window-height w))
	 )
    (set-buffer buffer)
    (erase-buffer)
    (insert s)
    (set-buffer cbuf)
    ))
    
;;; Send a NESL function to the pretty printer.
;;; Display it in the POPUP-WINDOW, or bring up NESL process window
;;; if a prefix arg is given.
(defun nesl-pretty-print-defun (display-flag)
  "Send the current function to the NESL process and pretty-print it
in a popup window.  Optional prefix arg means to bring up the NESL
process window instead of a popup."
  (interactive "P")
  (save-excursion
    (let* (
	   (begin (save-excursion
		    (beginning-of-nesl-function)
		    (point)))
	   (end (save-excursion
		  (end-of-nesl-function)
		  (forward-word -1)	; skip back before $
		  (point)))
	   (fn (buffer-substring begin end)) ; extract function def
	   )
      (process-send-string nesl-process (format "pprint(%s) $\n" fn))))
  (if display-flag
      (nesl-redisplay-process-window)
    (nesl-popup-window
     (nesl-get-process-output))))

  
;; Send a NESL function to be evaluated and find the TYPE returned.
;; Modify the current function so that the type is inserted.
(defun nesl-insert-function-type ()
  (interactive)
  (let* (
	 (bof)				; beginning of function
	 (eof				; end of function
	  (save-excursion
	    (end-of-nesl-function)
	    ;; skip back before $
	    (forward-word -1)
	    (point)))
	 (eoa)				; end of arg list
	 (eot)				; end of type information
	 )
    (save-excursion			; find various points in function
	(beginning-of-nesl-function)
	(setq bof (point))
	(forward-word 2)		; skip "function NAME"
	(forward-sexp 1)		; skip arg list
	(setq eoa (point))
	(skip-chars-forward " \t")
	(if (looking-at "=")
	    (setq eot (point))
	  (progn 
	    (search-forward "=" eof)
	    (forward-char -1)		; before the =
	    (setq eot (point))))
	)
    (process-send-string		; send the DEFUN to NESL interpreter
     nesl-process
     (format "%s $\n" (buffer-substring bof eof)))
			 
    (let* (
	  (output (nesl-get-process-output))
	  (type   (nesl-parse-defun-output output))
	  )
      (if type				; if non-nil, replace type clause
	  (progn
	    (goto-char eoa)
	    (delete-region eoa eot)	; delete the old
	    (insert "\n" type " ")	; insert the new
	    )))
    ))

;; If there is no error, return the type portion of a defun or NIL
;; if there was an error
(defun nesl-parse-defun-output (s)
  (save-window-excursion
    (set-buffer (get-buffer-create "*nesl junk buffer*"))
    (set-syntax-table nesl-mode-syntax-table) ; to parse words like NESL
    (erase-buffer)
    (insert s)				; write it here
    (beginning-of-buffer)		; back to the top
    (if (search-forward "Error in Function" (point-max) t)
	nil				; error condition
      (progn
	(beginning-of-buffer)
	(forward-word 1)		; skip fn name
	(forward-sexp 1)		; skip arts
	(buffer-substring (point)
			  (save-excursion
			    (end-of-buffer)
			    (skip-chars-backward " \n")
			    (point)))
	))))
      
	    
  
