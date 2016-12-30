(defpackage calculator
 (:use "COMMON-LISP")
 (:export :combine-expr :enclose-expression)
)

(in-package "CALCULATOR")

(defun combine-expr (operator operand expr)
 (cons (list operator operand (car expr)) (cdr expr))
)

(defun enclose-expression (expr)
  "Convert an expression in in-fix notation to Cambridge-Prefix
   notation."
  (check-type expr list)
  (if (null (rest expr)) expr
      (enclose-expression (combine-expr (second expr) (first expr) (rest (rest expr))))))

