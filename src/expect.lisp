
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
     :initform t
     :accessor successful-p)
   (reasons
     :initform nil
     :accessor reasons)))

(defun force-list (value)
  (typecase value
    (list value)
    (t (list value))))

(defmethod add-reason ((expect-container expect-container) reason)
  (set-unsuccessful *test-container*)
  (setf (successful-p *it-container*) nil)
  (setf (successful-p expect-container) nil)
  (setf (reasons expect-container)
    (append (reasons expect-container) (list reason))))

(defmethod run-assertion-with-flags
  ((expect-container expect-container)
   &key
   (to-be-truthy nil)
   (to-be-falsy nil)
   (to-equal nil)
   (to-have-been-called-with nil)
   (times nil))
  (setf (has-been-run-p expect-container) t)
  (let
    ((subject (subject expect-container)))
    (when (not (null to-have-been-called-with))
      (let*
        ((mock-index (mock-index *test-container*))
         (mock-container (gethash subject mock-index))
         (calls (calls mock-container))
         (matching-calls
          (match-call mock-container (force-list to-have-been-called-with)))
         (matches-calls-p (> (length matching-calls) 0)))
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
        (when (not is-equal-p)
          (add-reason expect-container
            (format nil "~A is not equal to ~A" subject to-equal)))))
    (when (not (null to-be-truthy))
      (let ((is-truthy-p (not (null subject))))
        (when (not is-truthy-p)
          (add-reason expect-container
            (format nil "~A is not truthy" subject)))))
    (when (not (null to-be-falsy))
      (let ((is-falsy-p (null subject)))
        (when (not is-falsy-p)
          (add-reason expect-container
            (format nil "~A is not falsy" subject)))))))

(defmacro run-assertion (expect-container flag &body body)
  (case flag
    ((:to-be-truthy
      :to-be-falsy)
     `(run-assertion-with-flags ,expect-container ,flag t))
    (t `(run-assertion-with-flags ,expect-container ,flag ,@body))))

(defmacro expect-with-canonical-subject (subject &body body)
  `(if (not *it-container*)
     (error "`expect` requires an `it` wrapper.")
     (let
       ((expect-container
         (make-instance (quote expect-container)
           :subject ,subject
           :it-container *it-container*)))
       (setf (expects *it-container*)
         (append (expects *it-container*) (list expect-container)))
       (run-assertion expect-container ,@body))))

(defmacro expect (subject &body body)
  (if (fboundp subject)
    `(expect-with-canonical-subject (quote ,subject) ,@body)
    `(expect-with-canonical-subject ,subject ,@body)))
