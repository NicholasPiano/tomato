
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
   (mocks
     :initform nil
     :accessor mocks)
   (mock-index
     :initform (make-hash-table)
     :accessor mock-index)
   (before
     :initform nil
     :accessor before)))

(defmethod do-mocks ((test-container test-container) mocks)
  (let
    ((first-mock (first mocks))
     (rest-mocks (rest mocks)))
    (when first-mock
      (do-mock first-mock)
      (do-mocks test-container rest-mocks))))

(defmethod undo-mocks ((test-container test-container) mocks)
  (let
    ((first-mock (first mocks))
     (rest-mocks (rest mocks)))
    (when first-mock
      (undo-mock first-mock)
      (undo-mocks test-container rest-mocks))))

(defmethod execute-before ((test-container test-container))
  (mapcar (lambda (before) (funcall before))
    (before test-container)))

(defmethod execute-its ((test-container test-container) its)
  (let
    ((first-it (first its))
     (rest-its (rest its)))
    (when first-it
      (execute-before test-container)
      (execute first-it)
      (execute-its test-container rest-its))))

(defmethod execute ((test-container test-container))
  (do-mocks test-container (mocks test-container))
  (execute-its test-container (its test-container))
  (undo-mocks test-container (mocks test-container)))

(defmethod add-mock ((test-container test-container) mock-container)
  (setf (gethash (mock-symbol mock-container) (mock-index test-container))
    mock-container)
  (setf (mocks test-container)
    (append (mocks test-container) (list mock-container))))

(defmethod add-before ((test-container test-container) before)
  (setf (before test-container)
    (append (before test-container) (list before))))

(defmacro test (description &body body)
  `(let
     ((*test-container*
       (make-instance (quote test-container)
         :description ,description
         :parent *test-container*)))
     ,@body
     (execute *test-container*)))
