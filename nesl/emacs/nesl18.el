(if (> (string-to-int (substring emacs-version 0 2)) 18)
    (progn
      (message "ERROR: The nesl18.el file is designed for emacs version 18 or earlier versions.")
      (sit-for 5)
      (top-level)))

(load-library "shell")

(defvar inferior-nesl-mode-map nil)

(if inferior-nesl-mode-map
    nil
  (setq inferior-nesl-mode-map (copy-alist shell-mode-map))
  (lisp-mode-commands inferior-nesl-mode-map)
  (define-key inferior-nesl-mode-map "\e\C-x" 'nesl-send-defun))

;;(if inferior-nesl-mode-map
;;    nil
;;  (setq inferior-nesl-mode-map (make-sparse-keymap))
;;  (define-key inferior-nesl-mode-map "\e\C-x" 'nesl-send-defun))

(defvar inferior-nesl-program "runnesl"
  "*Program name for invoking an inferior Nesl with `run-nesl'.")

(defvar inferior-nesl-load-command 
  "load(\"%s\", verbose := f, print := t, save_filename := f) $\n"
  "Command sent to Inferior NESL to load a file.")

(defvar inferior-nesl-prompt "^.*>:? *$")

(defun inferior-nesl-mode ()
  "Major mode for interacting with an inferior Nesl process.
Runs a Nesl interpreter as a subprocess of Emacs, with Nesl I/O
through an Emacs buffer.  Variable inferior-nesl-program controls
which Nesl interpreter is run.  

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{shell-mode-map}

Entry to this mode calls the value of nesl-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
nesl-mode-hook is called after shell-mode-hook.

You can send text to the inferior Nesl from other buffers
using the commands process-send-region, process-send-string
and \\[nesl-send-defun]."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-nesl-mode)
  (setq mode-name "Inferior Nesl")
  (setq mode-line-process '(": %s"))
  (lisp-mode-variables t)
  (use-local-map inferior-nesl-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (run-hooks 'shell-mode-hook 'nesl-mode-hook))

(defun run-nesl ()
  "Run an inferior Nesl process, input and output via buffer *nesl*."
  (interactive)
  (switch-to-buffer (make-shell "nesl" inferior-nesl-program))
  (inferior-nesl-mode))

