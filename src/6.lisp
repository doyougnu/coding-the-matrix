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

;; goes infinite
;; (depth-first-search 1 (is 12) #'binary-tree)

(defun prepend (x y)
  "Prepend y to start of x"
  (append y x))

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

(defun finite-binary-tree (n)
  "Return a successor function that generates a binary tree with n nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
                 (binary-tree x))))

(defun diff (num)
  "Return the function that finds the difference from num."
  #'(lambda (x) (abs (- x num))))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

;; (best-first-search 1 (is 12) #'binary-tree (diff 12))

(defun price-is-right (price)
  "Return a function that measure the difference from price. But gives a big
  penalty for going over price"
  #'(lambda (x) (if (> x price)
                    most-positive-fixnum
                    (- price x))))

;; (best-first-search 1 (is 12) #'binary-tree (price-is-right 12))

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reched, but never consider
  more than beam-width states at a time."
  (tree-search (list start) goal-p successors
               #'(lambda (old new)
                   (let ((sorted (funcall (sorter cost-fn) old new)))
                     (if (> beam-width (length sorted))
                         sorted
                         (subseq sorted 0 beam-width))))))

(beam-search 1 (is 12) #'binary-tree (price-is-right 12) 2)

;; on 206
