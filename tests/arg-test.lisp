
(defun a
  (&key
   (a nil)
   (b nil)
   (c nil))
  (format t "~A ~A ~A~%" a b c))

(defmacro am (flag &body body)
  (if (not body)
    `(a )
  )
  ; (case flag-or-first-key
  ;   ((:a :b) `(a ,flag-or-first-key t))
  ;   (t `(a ,flag-or-first-key ,@body)))
)

(am :a :c "hello")
