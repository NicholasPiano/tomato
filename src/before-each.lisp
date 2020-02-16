
(in-package :tomato)

(defmacro before-each (&body body)
  `(add-before *test-container*
     (lambda () ,@body)))
