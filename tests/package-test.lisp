
(defpackage :one (:use :cl) (:export hello))

(in-package :one)

(defun something () "Hello")

(defun hello () (something))

(defpackage :two (:use :cl :one))

(in-package :two)

(let
  ((one::something (lambda () "Goodbye")))
  (format t "~A~%" (hello)))
