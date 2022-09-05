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

(->> (list 1 2 3 4 5)
  (mapcar #'1+)
  (reduce #'+))

(defun png-from-file (f) (jupyter:file f :display t))
(defun function-plot (output)
  (with-plots (s :debug nil)
    (gp-setup :terminal '(pngcairo) :output output)
    (plot "(sin(1/x) - cos(x))*erfc(x)"))
  output)

(defun function-plot (output)
  (with-plots (s :debug nil)
    (gp-setup :terminal '(pngcairo) :output output)
    (plot "(sin(1/x) - cos(x))*erfc(x)"))
  output)

(png-from-file (function-plot "images/function-plot.png"))
