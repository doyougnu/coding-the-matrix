(in-package :cl-ctm)

(defvar *ops*   nil "A list of available operators")

(defstruct op "An operation"
           (action   nil)
           (preconds nil)
           (del-list nil)
           (add-list nil))

;; notice we bind *ops* to itself, the leading *ops* is actual a lexical binding
;; of the parameter which changes the *ops* that achieve-all references! Nice
;; little short hand here
(defun gps (state goals &optional (*ops* *ops*))
   "General Problem Solver; achieve all goals using provided *ops* even more text
    so much text"
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

(defun achieve-all (state goals goals-stack)
  "Achieve each goal, and make sure they still hold at they end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goals-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds.
  or if there is an appropriate op for it that is applicable."
  (dbg-ident :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state)      state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriate-p)))))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  "Print a message and update *state* if op is applicable."
  (dbg-ident :gps (length goal-stack) "Action: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; return an updated state
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;; Return something useful. but not too verbose:
  ;; the number of operators.
  (length (setf *ops* oplist)))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not
                 &allow-other-keys)
  "Find all elements of sequence that match item according to the keywords. Does
   not alter the sequence"
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test     (complement test)     keyword-args)))

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun my-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids)
                      nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-ident (id ident format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i ident) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defun executing-p (x)
  "Is x of the form: (executing ...)? "
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list)
       (eql (first list) x)))

(defun convert-op (op)
  "Makes op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention"
  (convert-op
   (make-op :action action
            :preconds preconds
            :add-list add-list
            :del-list del-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests
(make-op :action   'drive-son-to-school
         :preconds '(son-at-home car-works)
         :add-list '(son-at-school)
         :del-list '(son-at-home))

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
   (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))))


(my-debug :gps)
(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school))
