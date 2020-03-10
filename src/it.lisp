
(in-package :tomato)

(defparameter *it-container* nil)

(defclass it-container ()
  ((description
     :initarg :description
     :accessor description)
   (test-container
     :initarg :test-container
     :accessor test-container)
   (successful-p
     :initform t
     :accessor successful-p)
   (body
     :initarg :body
     :accessor body)
   (expects
     :initform nil
     :accessor expects)))

(defmethod execute ((it-container it-container))
  (let
    ((*it-container* it-container)
     (body (body it-container)))
    (funcall body)))

(defmethod report ((it-container it-container) &key single)
  (format t "~A ~Ait ~A~%"
    (if (successful-p it-container) "[PASSED]" "[FAILED]")
    (make-string
      (+ 2 (* 2 (depth (test-container it-container))))
      :initial-element #\Space)
    (description it-container))
  (mapcar (function report) (expects it-container)))

(defmacro it (description &body body)
  `(if (not *test-container*)
     (error "`it` statements should be created within a `test` wrapper")
     (let*
       ((it
         (make-instance (quote it-container)
           :description ,description
           :body (lambda () ,@body)
           :test-container *test-container*)))
        (setf (its *test-container*)
          (append (its *test-container*) (list it))))))
