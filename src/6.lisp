(in-package :cl-ctm)

(defun interactive-interpreter (prompt transformer)
  "Read an expression, transform it, and print the result."
  (loop
        (handler-case
            (progn
              (if (stringp prompt)
                  (print prompt)
                  (funcall prompt))
              (print (funcall transformer (read))))
          ;; in case we error
          (error (condition)
            (format t "~&;; Error ~a ignored. back to top level." condition)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
  "Return a function that prints prompts like [1], [2], etc."
  #'(lambda () (format t ctl-string (incf num))))

(defun compose (f g)
  "Return the function that computes (f (g x))"
  #'(lambda (x) (funcall f (funcall g x))))

;;; begin 6.4
;;; its interesting that symbols can just be setf'd to whatever they need to be
;;; and on't need to be declared first. How uncomfortably dynamic.
;;; -----
;;; just kidding I was confused about property lists which create maps from symbols
;;; see: https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node108.html

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p. Start with states, and search according
  to successors and combiner."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))


(defun binary-tree (x)
  "And example binary tree"
  (list (* 2 x) (+ 1 (* 2 x))))

(defun is (value)
  #'(lambda (x)
      (eql x value)))

(my-debug :search)
;; (depth-first-search 1 (is 12) #'binary-tree)
