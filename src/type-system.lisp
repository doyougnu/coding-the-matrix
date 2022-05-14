(in-package :cl-ctm)

(defvar universe (find-class 't))

;; the type class
(defclass TP ()
  ((name :type     symbol
         :initarg  :name
         :initform (gensym)
         :reader   name))
  (:metaclass sb-mop::funcallable-standard-class)
  )

(defmethod print-object ((tp tp) stream)
  (format stream
          "#<~S ~S>"
          (class-name (class-of tp)) (name tp)))

;; we always want to set the name of the instance object to the type descriptor
;; name
(defmethod initialize-instance :after ((tp tp) &rest rest)
  (declare (ignore rest))
  (set (name tp) tp))

;; a directed type class
;; notice the input of TP, which means DTP is a subclass
(defclass DTP (TP)
  ((sub :type list :accessor sub)  ;; a subclass
   (sup :type list :accessor sup))
  (:metaclass sb-mop::funcallable-standard-class)
  ) ;; a super class

;; now we can make Any and Void!
(defvar void (make-instance 'DTP :name 'void))

;; and Any is a subclass of Void
(setf (sub void) '(void) (sup void) '(void any))

;; Alright I got it figured out. I kept getting a cannot-change-class error in
;; the interpreter by playing around with the :metaclass instances around these
;; objects. i finally figured it out you cannot redefine on the fly like that
;; you need to either restart the interpreter or over write the object so I
;; restarted then it worked!
(sb-mop::set-funcallable-instance-function void
                                   #'(lambda (obj)
                                       (declare (type t obj))
                                       (the boolean nil)))
(funcall void 'anything)
