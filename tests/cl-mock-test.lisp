
(load "~/quicklisp/setup.lisp")
(ql:quickload :tomato)
(ql:quickload :cl-mock)

(defpackage :load
  (:use :cl :tomato :cl-mock))

(in-package :load)

(dflet
  ((tomato::hidden () "Goodbye"))
  (format t "~A~%" (hello)))
