(defpackage calculator
 (:use "COMMON-LISP")
 (:export "COMBINE-EXPR")
)
(in-package "CALCULATOR")
(defun combine-expr (operator operand expr)
 (cons (list operand operator (car expr)) (cdr expr))
)
(export 'combine-expr)
