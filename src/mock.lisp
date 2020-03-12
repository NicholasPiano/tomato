
(in-package :tomato)

(defclass call ()
  ((mock-container
     :initarg :mock-container
     :accessor mock-container)
   (arguments
     :initarg :arguments
     :accessor arguments)))

(defclass mock-container ()
  ((test-container
     :initarg :test-container
     :accessor test-container)
   (mock-symbol
     :initarg :mock-symbol
     :accessor mock-symbol)
   (original
     :initarg :original
     :accessor original)
   (mocked
     :initform nil
     :accessor mocked)
   (calls
     :initform nil
     :accessor calls)))

(defmethod add-call ((mock-container mock-container) arguments)
  (let
    ((call
       (make-instance (quote call)
         :mock-container mock-container
         :arguments arguments)))
    (setf (calls mock-container)
      (append (calls mock-container) (list call)))))

(defmethod do-mock ((mock-container mock-container))
  (let
    ((mock-symbol (mock-symbol mock-container))
     (mocked (mocked mock-container)))
    (setf (fdefinition mock-symbol) mocked)))

(defmethod undo-mock ((mock-container mock-container))
  (setf (fdefinition (mock-symbol mock-container)) (original mock-container)))

(defun list-equal (first-list second-list &key (fn nil))
  (let
    ((first-item (first first-list))
     (second-item (first second-list))
     (first-list (rest first-list))
     (second-list (rest second-list)))
    (if (and first-list second-list)
      (list-equal first-list second-list)
      (if fn
        (funcall fn first-item second-item)
        (equal first-item second-item)))))

(defmethod match-call ((mock-container mock-container) arguments)
  (let*
    ((calls (calls mock-container))
     (matching-calls
      (filter (lambda (call) (list-equal (arguments call) arguments))
        calls)))
    matching-calls))

(defmethod reset-mock ((mock-container mock-container))
  (setf (calls mock-container) nil))

(defmacro mock (mock-symbol &key (as nil))
  `(let*
     ((original (fdefinition (quote ,mock-symbol)))
      (mock-container
        (make-instance (quote mock-container)
          :test-container *test-container*
          :mock-symbol (quote ,mock-symbol)
          :original original))
      (mock-function
        (lambda (&rest rest)
          (add-call mock-container rest)
          (if ,as
            (apply ,as rest)
            (apply original rest)))))
     (setf (mocked mock-container) mock-function)
     (add-mock *test-container* mock-container)))

(defmacro mock-reset (mock-symbol)
  `(let*
     ((mock-index (mock-index *test-container*))
      (mock-container (gethash (quote ,mock-symbol) mock-index)))
     (reset-mock mock-container)))
