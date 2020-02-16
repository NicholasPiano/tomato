
(in-package :tomato)

(defclass expect-container ()
  ((subject
     :initarg :subject
     :accessor subject)
   (it-container
     :initarg :it-container
     :accessor it-container)
   (has-been-run-p
     :initform nil
     :accessor has-been-run-p)
   (successful-p
     :initform nil
     :accessor successful-p)
   (reasons
     :initform nil
     :accessor reasons)))

(defun force-list (value)
  (typecase value
    (list value)
    (t (list value))))

(defmethod add-reason ((expect-container expect-container) reason)
  (setf (reasons expect-container)
    (append (reasons expect-container) (list reason))))

(defmethod run-assertion
  ((expect-container expect-container)
   &key
   (to-equal nil)
   (to-have-been-called-with nil)
   (times nil))
  (setf (has-been-run-p expect-container) t)
  (let
    ((subject (subject expect-container)))
    (when to-have-been-called-with
      (let*
        ((mock-index (mock-index *test-container*))
         (mock-container (gethash subject mock-index))
         (calls (calls mock-container))
         (matching-calls
          (match-call mock-container (force-list to-have-been-called-with)))
         (matches-calls-p (> (length matching-calls) 0)))
        (setf (successful-p expect-container) matches-calls-p)
        (when (not matches-calls-p)
          (add-reason expect-container
            (format nil
              "Expected ~A to be called with `~A`,
              but it was called with `~A`."
              subject
              to-have-been-called-with
              (mapcar
                (lambda (call) (arguments call))
                calls))))
        (when (not (null times))
          (let*
            ((number-of-calls (length calls))
             (matches-times-p (equal times number-of-calls)))
            (setf (successful-p expect-container) matches-times-p)
            (when (not matches-times-p)
              (add-reason expect-container
                (format nil
                  "Expected ~A to be called ~A times,
                  but it was called ~A times."
                  subject
                  times
                  number-of-calls)))))))
    (when to-equal
      (let
        ((is-equal-p (equal subject to-equal)))
        (setf (successful-p expect-container) is-equal-p)
        (when (not is-equal-p)
          (add-reason expect-container
            (format t "~A is not equal to ~A" subject to-equal)))))))

(defmacro expect (subject &body body)
  `(if (not *it-container*)
     (error "`expect` requires an `it` wrapper.")
     (let
       ((expect-container
         (make-instance (quote expect-container)
           :subject ,subject
           :it-container *it-container*)))
       (run-assertion expect-container ,@body))))

(defmacro fexpect (subject &body body)
  `(expect (quote ,subject) ,@body))
