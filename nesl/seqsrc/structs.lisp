;;;
;;; Copyright (c) 1992, 1993, 1994 Carnegie Mellon University
;;; All Rights Reserved.
;;;
;;; See COPYRIGHT file for further information.
;;;

(in-package :nesl-lisp)

(defstruct closure
  env
  arg
  body
  other
)

(defstruct binding
  name
  value
  type
  datatype
  original-code
)

(defstruct nesl-seq
  len
  value
  type
)

(defstruct datatype
  name
  value
  type
  arg-type
)


(defstruct func
  name
  arguments
  type
  body
  other)

(defstruct typedef
  name
  type
  documentation
  redefine)
