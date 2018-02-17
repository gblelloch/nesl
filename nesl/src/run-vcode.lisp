;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

;;;
;;; Code for running VCODE process. 
;;; (Original code written by Timothy Freeman.  Major modifications
;;;  by Guy Blelloch.)
;;;

(in-package :nesl-lisp) 

;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINITION OF CONFIGURATION STRUCTURE.
;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct config
  name
  memory-size
  interp-file
  machine-name
  rsh-command
  plot-file
  max-time
  arguments
  temp-dir
  background-command
  foreground-command)

(defun print-config (config stream)
  (format stream "~%Configuration Name: ~s~
                  ~%interp_file:        ~s~
                  ~%memory_size:        ~s~
                  ~%temp_dir:           ~s~
       	          ~%plot_file:          ~s"
	  (string-downcase (config-name config))
	  (config-interp-file config) (config-memory-size config)
	  (config-temp-dir config) (config-plot-file config))
  (when (plusp (length (config-rsh-command config)))
    (format stream "~%rsh_command:        ~s" (config-rsh-command config))
    (format stream "~%machine_name:       ~s" (config-machine-name config)))
  (when (not (string= "foreground-unix" (config-foreground-command config)))
    (format stream "~%foreground_command: ~s"
	    (config-foreground-command config)))
  (when (not (string= "background-unix" (config-background-command config)))
    (format stream "~%background_command: ~s"
	    (config-background-command config)))
  (when (plusp (length (config-arguments config)))
    (format stream "~%arguments:          ~s" (config-arguments config)))
  (when (not (minusp (config-max-time config)))
    (format stream "~%max_time:           ~s" (config-max-time config))))

(defun list-configs (config-list)
  (format t "~%The current machine configurations are:")
  (mapcar #'(lambda (a) (format t "~%~a" (car a))) (reverse config-list))
  (format t "~%To use one type: set config <config>;")
  nil)

(defun set-memsize (size config)
  (when (not (integerp size))
    (nesl-error "The size argument to SET-MEMSIZE must be an integer."))
  (setf (config-memory-size config) size))

(defun set-interpreter-file (filename config)
  (setf (config-interp-file config) filename))

(defun set-rsh-command (command config)
  (when (not (stringp command))
    (nesl-error "The rsh_command must be a string."))
  (setf (config-rsh-command config) command))

(defun get-configuration (config-name)
  (declare (special *config-list*))
  (or (cdr (assoc config-name *config-list*))
      (nesl-error "~%~a is not a valid configuration.~%~
                     Use \"show configs\" to list the configurations."
		  config-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  WRITING A VCODE FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-vcode-file (topsym codefile definitions)
  (write-func topsym '() definitions codefile))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  READING VCODE RESULTS
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-line (stream)
  (let ((list (read stream nil 0)))
    (when (not (listp list))
      (nesl-error "Error while running VCODE."))
    list))

(defun check-float-line (list)
  (dolist (elt list)
    (when (not (floatp elt))
      (nesl-error 
       "Returning ~a: infinity and not-a-number (NAN) are not supported."
       elt)))
  list)

(defun parse-segdes-line (stream)
  (let ((left-bracket (read stream nil 0)))
    (when (not (eql left-bracket '[))
      (nesl-error "Error while running VCODE."))
    (read-delimited-list #\] stream)))

(defun size-message (len)
  (cond ((and len (> len 200000))
	 (format 
	  t 
	  "~%Warning: Result is very large, it will take a while to read.~%~
         Use file variables, as in <pattern> |= <exp>; to save such results."))
	((and len (> len 20000))
	 (format 
	  t 
	  "~%Warning: Result is large, it will take a while to read."))))

(defun read-vcode-result (outfile result-types)
  (declare (special *verbose* *debug*))
  (when *verbose* (format t "Reading..") (force-output))
  (let ((result 
	 (with-open-file (s outfile :direction :input)
	   (let ((result nil))
	     (size-message (file-length s))
	     (dolist (type result-types)
		     (push
		      (cond
		       ((member type '(int bool char))
			(parse-line s))
		       ((eql type 'float)
			;; checks for not-a-number and infinity
			(check-float-line (parse-line s)))
		       ((eql type 'segdes)
			(parse-segdes-line s))
		       (t (nesl-error "Can't handle the result type ~s." 
				      type)))
		      result))
	     (nreverse result)))))
    (when (not *debug*) (delete-file outfile))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  GENERAL UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *runcount* 0)
(defparameter *username* nil)

(defun user-name ()
  (if *username* *username*
    (let ((getenv (getenv "USER")))
      (setq *username* 
	    (if getenv getenv
	      (progn
		(format t "What is your user name? ")
		(read-line)))))))

(defun delete-if-exists (filename)
  (when (probe-file filename) (delete-file filename)))

(defun job-name ()
  (format nil "vcode_~a_~a" (user-name) (incf *runcount*)))

(defun fname (config jobname tailer)
  (format nil "~a~a_~a" (config-temp-dir config) jobname tailer))

(defun remote-shell (command config)
  (run-shell-line
   (format nil "~a \"~a\"" (config-rsh-command config) command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  FOREGROUND PROCESSING
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-vcode-script (script rsh interp memsize tempdir 
				jobname maxtime args)
  (let ((script (if (probe-file script)
		    (concatenate 'string "./" script)
		  (concatenate 'string "bin/" script))))
    (run-shell-line 
     (format nil "~a ~s ~a ~a ~a ~a ~a ~s"
	     script rsh interp memsize tempdir jobname maxtime args))))

(defun run-vcode (topsym result-types config definitions)
  (declare (special *verbose*))
  (let* ((jobname (job-name))
	 (outfile (fname config jobname "out"))
	 (codefile (fname config jobname "code")))
    (delete-if-exists outfile)
    (write-vcode-file topsym codefile definitions)
    (when *verbose* 
      (if (zerop (length (config-machine-name config)))
	  (format t "Loading..")
	(format t "Loading on ~a..~%" (config-machine-name config)))
      (force-output))
    (run-vcode-script (config-foreground-command config)
		      (config-rsh-command config)
		      (config-interp-file config)
		      (config-memory-size config)
		      (config-temp-dir config)
		      jobname
		      (config-max-time config)
		      (config-arguments config))
    (delete-if-exists codefile)
    (when (not (probe-file outfile))
      (nesl-error "Error while executing VCODE."))
    (read-vcode-result outfile result-types)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  BACKGROUND PROCESSING
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-vcode-background (topsym memsize maxtime config definitions)
  (declare (special *verbose*))
  (let* ((jobname (job-name))
	 (outfile (fname config jobname "out"))
	 (codefile (fname config jobname "code"))
	 (errfile (fname config jobname "err"))
	 (checkfile (fname config jobname "check")))
    (write-vcode-file topsym codefile definitions)
    (delete-if-exists outfile)
    (delete-if-exists errfile)
    (delete-if-exists checkfile)
    (if *verbose* 
	(format t "Submitting (~a, maxtime = ~a sec.)..~%"
		(config-machine-name config) maxtime)
      (format t "~%"))
    (run-vcode-script (config-background-command config)
		      (config-rsh-command config)
		      (config-interp-file config)
		      memsize 
		      (config-temp-dir config) 
		      jobname
		      maxtime
		      (config-arguments config))
    (list outfile errfile checkfile codefile config)))

(defun read-vcode-err-file (errfile)
  (with-open-file (errstr errfile :direction :input)
    (loop 
     (multiple-value-bind (line eof) 
       (read-line errstr nil)
       (if (or (not line) eof)
	   (progn
	     (when line (write-string line))
	     (return))
	 (write-line line)))))
  (delete-file errfile))

;; checks if completed.
(defun check-background-result (out-err-files)
  (and (probe-file (third out-err-files))     ;; the check file
       (probe-file (second out-err-files))))  ;; the error file

(defun get-background-result (out-err-files result-types config)
  (declare (special *verbose*) (ignore config))
  (delete-file (third out-err-files))  ;; the check file
  (delete-file (fourth out-err-files)) ;; the code file
  (when *verbose* (format t "~%Getting background result....~%") 
	(force-output))
  (read-vcode-err-file (second out-err-files))
  (read-vcode-result (first out-err-files) result-types))
