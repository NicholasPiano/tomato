
(in-package :tomato)

(defparameter *test-container* nil)

(defclass test-container ()
  ((description
     :initarg :description
     :accessor description)
   (parent
     :initarg :parent
     :accessor parent)
   (tests
     :initform nil
     :accessor tests)
   (its
     :initform nil
     :accessor its)
   (successful-p
     :initform t
     :accessor successful-p)
   (mocks
     :initform nil
     :accessor mocks)
   (mock-index
     :initform (make-hash-table)
     :accessor mock-index)
   (before
     :initform nil
     :accessor before)
   (depth
     :initarg :depth
     :reader depth)))

(defmethod execute-before ((test-container test-container))
  (let ((*test-container* (parent test-container)))
    (when *test-container*
      (execute-before *test-container*)))
  (mapcar (function funcall) (before test-container)))

(defmethod execute ((test-container test-container))
  (let
    ((*test-container* test-container))
    (mapcar (function do-mock) (mocks test-container))
    (mapcar
      (lambda (it)
        (execute-before test-container)
        (execute it))
      (its test-container))
    (mapcar (function execute) (tests test-container))
    (mapcar (function undo-mock) (mocks test-container))))

(defmethod set-unsuccessful ((test-container test-container))
  (setf (successful-p test-container) nil)
  (when (parent test-container)
    (set-unsuccessful (parent test-container))))

(defmethod add-mock ((test-container test-container) mock-container)
  (setf (gethash (mock-symbol mock-container) (mock-index test-container))
    mock-container)
  (setf (mocks test-container)
    (append (mocks test-container) (list mock-container))))

(defmethod add-before ((test-container test-container) before)
  (setf (before test-container)
    (append (before test-container) (list before))))

(defmethod match-mock-call ((test-container test-container) subject query)
  (let*
    ((mock-index (mock-index test-container))
     (mock-container
      (when mock-index
        (gethash subject mock-index)))
     (calls
      (when mock-container
        (calls mock-container)))
     (matching-calls
      (when calls
        (match-call mock-container (force-list query))))
     (parent
      (parent test-container)))
    (or
      matching-calls
      (when parent (match-mock-call parent subject query)))))

(defmethod report ((test-container test-container) &key (single nil))
  (let
    ((depth (depth test-container))
     (description (description test-container))
     (successful-p (successful-p test-container))
     (it-containers (its test-container))
     (test-containers (tests test-container)))
    (format t "~A ~ATEST: ~A~%"
      (if successful-p "[PASSED]" "[FAILED]")
      (make-string
        (* 2 depth)
        :initial-element #\Space)
      description)
    (when (or single (not successful-p))
      (mapcar (function report) it-containers)
      (mapcar (function report) test-containers))))

(defmacro test (description &body body)
  `(let
     ((*test-container*
       (make-instance (quote test-container)
         :description ,description
         :parent *test-container*
         :depth
           (if *test-container*
             (+ 1 (depth *test-container*))
             0))))
     ,@body
     (if (null (parent *test-container*))
       (setf (test-containers *suite*)
         (append (test-containers *suite*) (list *test-container*)))
       (setf (tests (parent *test-container*))
         (append (tests (parent *test-container*)) (list *test-container*))))))
