(in-package :cl-ctm)


;; complex numbers
#C(0 1)
(complex 1 2)

(defun foo ()
  (+ 1 2))

(defclass mixin ()
  ((slot :initarg slot)))

(defclass func-obj (sb-mop::funcallable-standard-object mixin)
  ()
  (:metaclass sb-mop::funcallable-standard-class))
