(when (not (find-package 'nesl-lisp))
  (make-package 'nesl-lisp))

(in-package :nesl-lisp)

(defparameter *nesl-version* "3.1.1")

(defparameter *nesl-date* "November 1, 1995")

(defparameter *NESL-PATH* 
   (if (boundp 'user::*nesl-path*)
       user::*nesl-path*
     (namestring *default-pathname-defaults*)))

(defparameter *bindir* "neslbin/")
(defparameter *srcdir* "src/")

(load (concatenate 'string *nesl-path* "src/make-nesl.lisp"))
(load (concatenate 'string *nesl-path* "src/load-nesl.lisp"))

(when (and (boundp *fully-loaded*) *fully-loaded*)
  (format t "

; --> To start NESL type: (nesl) <--

"))
