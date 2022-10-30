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

(defun is (value &key (key #'identity) (test #'eql))
  "Returns a predicate that tests for a given value."
  #'(lambda (path) (funcall test value (funcall key path))))

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

;; (beam-search 1 (is 12) #'binary-tree (price-is-right 12) 2)

;; a city is a list of three elements
(defstruct (city (:type list)) name long lat)

(defparameter *cities*
  '((Atlanta 84.23 33.45) (Los-Angeles 118.15 34.03)
    (Boston  71.05 42.21) (Memphis     90.03 35.09)
    (Chicago 87.37 41.50) (New-York     73.58 40.47)
    (Denver  105.00 39.45) (Oklahoma-City 97.28 35.26)
    (Eugene  123.05 44.03) (Pittsburgh    79.57 40.27)
    (Flagstaff 111.41 35.13) (Quebec 71.11 46.49)
    (Grand-Jct 108.37 39.05) (Reno 119.49 39.30)
    (Houston 105.00 34.00)   (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46) (Tampa 82.27 27.57)
    (Jacksonville 81.40 30.22) (Victoria 123.21 48.25)
    (Kansas-City  94.35 39.06) (Wilmington 77.57 34.14)))

(defun neighbors (city)
  "Find all cities within 1000 kilometers."
  (find-all-if #'(lambda (c)
                   (and (not (eq c city))
                        (< (air-distance c city) 1000.0)))
               *cities*))

(defun city (name)
  "Find the city with this name."
  (assoc name *cities*))

(defun trip (start dest &optional (beam-width 1))
  "Search for the best path from the start to dest."
  (beam-search
    (make-path :state start)
    (is dest :key #'path-state)
    (path-saver #'neighbors #'air-distance
                #'(lambda (c) (air-distance c dest)))
    #'path-total-cost
    beam-width))

(defstruct (path (:print-function print-path))
  state (previous nil) (cost-so-far 0) (total-cost 0))

(defconstant earth-diameter 12765.0
  "Diameter of the earth in km")

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is a straight line chord between two cities.
    ;; the length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere. The center is (0 0 0)
  and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list (* (cos psi) (cos phi)
             (cos psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two points."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

(defun deg->radians (deg)
  (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))

;; going to skip to chapter 8
