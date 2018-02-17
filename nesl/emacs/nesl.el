(if (< (string-to-int (substring emacs-version 0 2)) 19)
    (progn
      (message "ERROR: The nesl.el file is designed for emacs version 19 or later versions.")
      (sit-for 5)
      (top-level)))

(defvar inferior-nesl-mode-map nil)

(require 'comint)

(if inferior-nesl-mode-map
    nil
  (setq inferior-nesl-mode-map (copy-keymap comint-mode-map)))

(defvar inferior-nesl-program "runnesl"
  "*Program name for invoking an inferior Nesl with `run-nesl'.")

(defvar inferior-nesl-load-command 
  "load(\"%s\", verbose := f, print := t, save_filename := f) $\n"
  "Command sent to Inferior NESL to load a file.")

(defvar inferior-nesl-prompt "^<Nesl> ")

(defvar inferior-nesl-filter-regexp ""
  "*What not to save on inferior Nesl's input history.
Input matching this regexp is not saved on the input history in Inferior Nesl
mode.  Default is nothing")


(defun inferior-nesl-mode () 
  "Major mode for interacting with an inferior Nesl process.  
Runs a Nesl interpreter as a subprocess of Emacs, with Nesl I/O through an
Emacs buffer.  Variable `inferior-nesl-program' controls which Nesl interpreter
is run.  Variables `inferior-nesl-prompt', `inferior-nesl-filter-regexp' and
`inferior-nesl-load-command' can customize this mode for different Nesl
interpreters.

\\{inferior-nesl-mode-map}

Customisation: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-nesl-mode-hook' (in that order).

You can send text to the inferior Nesl process from other buffers containing
Nesl source.  
    nesl-send-defun sends the current defun to the Nesl process.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Nesl; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inferior-nesl-prompt)
  (setq major-mode 'inferior-nesl-mode)
  (setq mode-name "Inferior Nesl")
  (setq mode-line-process '(": %s"))
  ;;(lisp-mode-variables t)
  (use-local-map inferior-nesl-mode-map)    ;c-c c-k for "kompile" file
  (setq comint-get-old-input (function nesl-get-old-input))
  (setq comint-input-filter (function nesl-input-filter))
  (setq comint-input-sentinel 'ignore)
  (run-hooks 'inferior-nesl-mode-hook))

(defun nesl-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun nesl-input-filter (str)
  "t if STR does not match `inferior-nesl-filter-regexp'."
  (or (= (length inferior-nesl-filter-regexp) 0)
      (not (string-match inferior-nesl-filter-regexp str))))

(defun run-nesl (cmd)
  "Run an inferior Nesl process, input and output via buffer *nesl*.
If there is a process already running in *nesl*, just switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-nesl-program').  Runs the hooks from
`nesl-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run nesl: " inferior-nesl-program)
		       inferior-nesl-program)))
  (if (not (comint-check-proc "*nesl*"))
      (let ((cmdlist (inferior-nesl-args-to-list cmd)))
	(set-buffer (apply (function make-comint)
			   "nesl" (car cmdlist) nil (cdr cmdlist)))
	(inferior-nesl-mode)))
  (setq nesl-buffer "*nesl*")
  (switch-to-buffer "*nesl*"))

;;; Break a string up into a list of arguments.
;;; This will break if you have an argument with whitespace, as in
;;; string = "-ab +c -x 'you lose'".
(defun inferior-nesl-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (inferior-nesl-args-to-list (substring string (+ 1 where)
							(length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (inferior-nesl-args-to-list (substring string pos
							(length string)))))))))

