
(in-package :tomato)

(defclass suite ()
  ((test-containers
     :initform nil
     :accessor test-containers)))

(defmethod run ((suite suite) &optional (query nil))
  (let*
    ((test-containers (test-containers suite))
     (matching-test-containers test-containers)
     (single (eq 1 (length test-containers))))
    (mapcar (function execute) test-containers)
    (mapcar
      (lambda (test-container) (report test-container :single single))
      test-containers)))

(defparameter *suite* (make-instance (quote suite)))
