
(in-package :tomato)

(defparameter *it-container* nil)

(defclass it-container ()
  ((description
     :initarg :description
     :accessor description)
   (test-container
     :initarg :test-container
     :accessor test-container)
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
