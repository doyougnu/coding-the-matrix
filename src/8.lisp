(in-package :cl-ctm)

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar #'prefix->infix
              (if (binary-exp-p exp)
                  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                  exp))))

(defun infix->prefix (infix-exp)
  "Convert fully parenthesized infex-exp to a prefix expression"
  ;; don't use this for non-fully parens'd expr
  (prefix->infix infix-exp))

;; on 250
