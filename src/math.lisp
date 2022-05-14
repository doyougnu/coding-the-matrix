(in-package :cl-ctm)

;; this is just a tutorial for CLOS going deriving the standard math hiearchy
(defclass C1 ()
  ;; sl1 is a _slot_
  ((sl1 :initarg :sl1 :reader sl1)))


(defclass C2 (C1) ;; notice inheritance is similar to parameterization of
                  ;; function defs!
  ;; C2 has 2 slots, sl1 (inherited) and sl2
  ((sl2 :initarg :sl2 :reader sl2)))


;; now we can create instances
;; notice:
;; ;; c1 is lower case and we _had_ to supply and key value argument for :sl1
;; ;; we use setf to set a variable just like any other variable
;; (setf i1 (make-instance 'c1 :sl1 1))

;; now its a global, sbcl complains about setf used in a global namespace and
;; rightly so
(defvar i1 (make-instance 'c1 :sl1 1))

(defvar i2 (make-instance 'c2 :sl1 11
                              :sl2 22))

i1
i2
;; the default display
 ; => #<C2 {10035736E3}>

;;;; but you can set specific displays with a generic function

(defmethod print-object ((c1 c1) stream)
  (declare (type stream stream))
  (format stream "#<~S sl1=~S" (class-name (class-of c1)) (sl1 c1))
  c1)

;; notice the :after there, which allows printing the >
(defmethod print-object :after ((c1 c1) stream)
  (declare (type stream stream))
  (format stream ">"))

;; and this is the print method for C2
(defmethod print-object ((c2 c2) stream)
  (declare (type stream stream))
  (call-next-method)
  (format stream " sl2=~S" (sl2 c2))
  c2)

(list i1 i2)


;; use , e f to eval!

;; we can mutate objects by supplying the missing slots
(change-class i1 'c2 :sl2 2)

;; we can also use around like in emacs
(defmethod print-object :around ((c1 c1) stream)
  (declare (type stream stream))
  (format stream "*")
  (call-next-method)
  (format stream "*")
  c1)
