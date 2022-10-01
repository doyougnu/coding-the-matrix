(in-package :cl-ctm)
;; let's make eliza

;; notice that we define a value for fail
(defconstant fail nil "Indicates a pat-match failure")

;; similarly this is basically the nil case of a list, but its value to end
;; recursion on
(defparameter no-bindings '((t . t))
  "Indicates a pat-match success, with no variables")

(defun simple-equal (x y)
  "Are x and y equal? (Don't check inside strings."
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
           (simple-equal (rest  x) (rest  y)))))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; we assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              fail
              (let ((b2 (pat-match pat (subseq input pos)
                                   (match-variable var (subseq input 0 pos) bindings))))
                ;; if this match failed, try another longer one
                ;; if it worked, check that the variables match
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Does pattern match input? Any variable can match anything"
  (cond ((eq bindings fail)          fail)
        ((variable-p pattern)        (match-variable pattern input bindings))
        ((eql pattern input)         bindings)
        ((segment-pattern-p pattern) (segment-match pattern input bindings))
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t fail)))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x)
       (equal (char (symbol-name x) 0) #\?)))

;; these functions lift the implementation into the semantic domain
(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list"
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding"
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; once we add a "real" binding
        ;; we can get rid of the dummy binding
        (if (eq bindings no-bindings)
            nil
            bindings)))

;;;;;;;;;;;;;;;;; Eliza ;;;;;;;;;;;;;;;;;;;;;;;
(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *eliza-rules*
  '((((?* ?x) hello (?* ?y))
    (How do you do. Please state your problem.))

   (((?* ?x) I want (?* ?y))
    (What would it mean if you got ?y)
    (Why do you want ?y)
    (Suppose you got ?y soon))

   (((?* ?x) if (?* ?y))
    (Do you really think its likely that ?y)
    (Do you wish that ?y)
    (What do you think about ?y)
    (Really-- if ?y)

    (((?* ?x) no (?* ?y))
     (Why not?)
     (You are being a bit too negative)
     (Are you saying "NO" just to be negative?))

    (((?* ?x) I was (?* ?Y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))

    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))

    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?)))))

(defun eliza ()
  "Respond to user input using pattern matching rules"
  (loop
    (print 'eliza>)
    (write (flatten (use-eliza-rules (read))) :pretty t)))

(defun use-eliza-rules (input)
  "Find some rule with which to transform the input"
  (some #'(lambda (rule)
            (let ((result (pat-match (rule-pattern rule) input)))
              (if (not (eq result fail))
                  (sublis (switch-viewpoint result)
                          (random-elt (rule-responses rule))))))
        *eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on"
  (sublis '((I . you) (you . I) (me . you) (am .are))
          words))

;; (defun random-elt (choices)
;;   "Choose an element from choices at random"
;;   (elt choices (random (length choices))))

(defun random-elt (choices)
  "Choose an element from choices at random"
  (-> (length choices)
      random
      (elt choices)))
