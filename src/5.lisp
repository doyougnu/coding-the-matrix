(in-package #:cl-ctm)
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
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              fail
              (let ((b2 (pat-match pat (subseq input pos)
                                   (match-variable var (subseq input 0 pos) bindings))))
                ;; if this match failed, try another longer one
                ;; if it worked, check that the variables match
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input, starting at
  position start. If pat1 is non-constant, then just return start"
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((< start (length input)) start)
        (t nil)))

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

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
   E.g. (?is x predicate) (?and .patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x. if it is a symbol that has one"
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x. if it is a symbol that has one"
  (when (symbolp x) (get x 'single-match)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns)
                      input
                      (pat-match (first patterns) input bindings)))))


(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns)
                                     input bindings)))
        (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input. This never binds any
   variables"
  (if (match-or patterns input  bindings)
      fail
      bindings))

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

(defun make-binding (var val) (cons var val))

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

(defun switch-viewpoint (words)
  "Change I to you and vice versa, and so on"
  (sublis '((I . you) (you . I) (me . you) (am .are))
          words))

(defun random-elt (choices)
  "Choose an element from choices at random"
  (-<> (length choices)
      random
      (elt choices <>)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input"
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables. The pattern looks like
  ((?if code) . rest)."
  (and (progv (mapcar #'car bindings)
              (mapcar #'cdr bindings)
         (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
        (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))


(defun rule-based-translator (input rules &key
                                            (matcher #'pat-match)
                                            (rule-if #'first)
                                            (rule-then #'rest)
                                            (action #'sublis))
  "Find the first rule in rules that matches input, and apply the action to that
   rule."
  (some #'(lambda (rule)
            (let ((result (funcall matcher (funcall rule-if rule)
                                   input)))
              (if (not (eq result fail))
                  (funcall action result (funcall rule-then rule)))))
        rules))

(defun use-eliza-rules (input)
  "Find some rules with which to transform the input."
  (rule-based-translator input *eliza-rules*
                         :action #'(lambda (bindings responses)
                                     (sublis (switch-viewpoint bindings)
                                             (random-elt responses)))))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisifes pred, where var-and-pred is the
  list (var pred)."
  (let  ((var          var-and-pred)
         (pred         var-and-pred))
    (let ((new-bindings (pat-match var input bindings)))
      (if (or (eq new-bindings fail)
              (not (funcall pred input)))
          fail
          new-bindings))))
