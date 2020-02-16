
(load "~/quicklisp/setup.lisp")
(ql:quickload :tomato)

(defpackage :load
  (:use :cl :tomato))

(in-package :load)

(defun function1 (y) (format nil "Hello, ~A!" y))
(defun function2 (x)
  (if x
    (function1 "friend")
    "Goodbye"))

(test "function2"
  (mock function1)
  (before-each
    (mock-reset function1))
  (it "calls function1 if x is true"
    (function2 t)
    (fexpect function1 :to-have-been-called-with "friend" :times 1))
  (it "returns Goodbye if x is false"
    (let
      ((result (function2 nil)))
      (expect result :to-equal "Goodbye"))))
