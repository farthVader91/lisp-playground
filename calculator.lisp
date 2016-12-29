(defpackage calculator
 (:use "COMMON-LISP")
 (:export "COMBINE-EXPR")
)

(in-package "CALCULATOR")

(defun combine-expr (operator operand expr)
 (cons (list operator operand (car expr)) (cdr expr))
)

(defun enclose-expression1 (expr last)
  "Convert an expression in in-fix notation to Cambridge-Prefix
   notation."
  (check-type expr list)
  (cond ((null expr) 
    (or
       (string= (symbol-name (first expr) "+"))
       (string= (symbol-name (first expr) "-"))
      )
