
(in-package :tomato)

(defclass expect-container ()
  ((subject
     :initarg :subject
     :accessor subject)
   (name
     :initarg :name
     :accessor name)
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

(defmethod report ((expect-container expect-container) &key single)
  (mapcar
    (lambda (reason)
      (format t "~A- ~A~%"
        (make-string
          (+ 13 (* 2 (depth (test-container (it-container expect-container)))))
          :initial-element #\Space)
        reason))
    (reasons expect-container)))

(defmethod add-reason ((expect-container expect-container) reason)
  (set-unsuccessful *test-container*)
  (setf (successful-p *it-container*) nil)
  (setf (successful-p expect-container) nil)
  (setf (reasons expect-container)
    (append (reasons expect-container) (list reason))))

(defmethod assert-called ((expect-container expect-container) &key invert)
  (let*
    ((subject (subject expect-container))
     (calls (mock-calls *test-container* subject)))
    (if invert
      (when calls
        (add-reason expect-container
          (format nil
            "Expected ~A not to be called, but it was called with [~A]."
            subject
            (mapcar (lambda (call) (arguments call)) calls))))
      (when (not calls)
        (add-reason expect-container
          (format nil
            "Expected ~A to be called, but it was not."
            subject))))))

(defmethod assert-called-with
  ((expect-container expect-container) &key with invert)
  (let*
    ((subject (subject expect-container))
     (mock-result (mock-calls-if-no-match *test-container* subject with))
     (calls (second mock-result)))
    (if invert
      (when (not calls)
        (add-reason expect-container
          (format nil
            "Expected ~A not to be called with [~A], but it was."
            subject
            with)))
      (when calls
        (add-reason expect-container
          (format nil
            "Expected ~A to be called with [~A],
            but it was called with [~A]."
            subject
            with
            (mapcar (lambda (call) (arguments call)) calls)))))))

(defmethod assert-called-times
  ((expect-container expect-container) &key times invert)
  (let*
    ((subject (subject expect-container))
     (calls (length (mock-calls *test-container* subject))))
    (if invert
      (when (equal calls times)
        (add-reason expect-container
          (format nil
            "Expected ~A not to be called [~A] times, but it was."
            subject
            times)))
      (when (not (equal calls times))
        (add-reason expect-container
          (format nil
            "Expected ~A to be called [~A] times,
            but it was called [~A] times."
            subject
            times
            calls))))))

(defmethod assert-equal
  ((expect-container expect-container) &key to using invert)
  (let
    ((subject (subject expect-container))
     (name (name expect-container))
     (eq-fn (or using (function equal))))
    (if invert
      (when (funcall eq-fn subject to)
        (add-reason expect-container
          (format nil
            "Expected ~A not to be equal to [~A], but it was."
            name
            to)))
      (when (not (funcall eq-fn subject to))
        (add-reason expect-container
          (format nil
            "Expected ~A to be equal to [~A],
            but it was equal to [~A]."
            name
            to
            subject))))))

(defmethod run-assertion-with-flags
  ((expect-container expect-container)
   &key
   (invert nil)
   (to-be-truthy nil)
   (to-be-falsy nil)
   (to-have-been-called nil)
   (to-equal nil)
   (using nil)
   (to-have-been-called-with nil)
   (times nil)
   (to-have-been-called-times nil))
  (setf (has-been-run-p expect-container) t)
  (let
    ((subject (subject expect-container))
     (name (name expect-container)))
    (when to-have-been-called
      (assert-called expect-container :invert invert))
    (when (not (null to-have-been-called-with))
      (assert-called-with expect-container
        :with to-have-been-called-with
        :invert invert))
    (when (or (not (null times)) (not (null to-have-been-called-times)))
      (assert-called-times expect-container
        :times (or times to-have-been-called-times)
        :invert invert))
    (when to-equal
      (assert-equal expect-container
        :to to-equal
        :using using
        :invert invert))
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

(defmacro run-assertion (expect-container flag &optional (value nil) &body body)
  (if value
    (typecase value
      (keyword
       (case flag
         ((:to-be-truthy
           :to-be-falsy
           :to-have-been-called)
          `(run-assertion ,expect-container ,value ,@body ,flag t))
         (:not `(run-assertion ,expect-container ,value ,@body :invert t))))
      (t `(run-assertion-with-flags ,expect-container ,flag ,value ,@body)))
    `(run-assertion-with-flags ,expect-container ,flag t)))

(defmacro expect-with-canonical-subject (subject &body body)
  `(if (not *it-container*)
     (error "`expect` requires an `it` wrapper.")
     (let
       ((expect-container
         (make-instance (quote expect-container)
           :subject ,subject
           :name (quote ,subject)
           :it-container *it-container*)))
       (setf (expects *it-container*)
         (append (expects *it-container*) (list expect-container)))
       (run-assertion expect-container ,@body))))

(defmacro expect (subject &body body)
  (if (fboundp subject)
    `(expect-with-canonical-subject (quote ,subject) ,@body)
    `(expect-with-canonical-subject ,subject ,@body)))
