(defpackage calculator
 (:use "COMMON-LISP")
 (:export "COMBINE-EXPR")
)
(in-package "CALCULATOR")
(defun combine-expr (operator operand expr)
 (cons (list operator operand (car expr)) (cdr expr))
)
(export 'combine-expr)
