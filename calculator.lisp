(defpackage calculator
 (:use :cl)
 (:export combine-expr enclose-expression enclose-term enclose-factor)
)

(in-package :calculator)

(defun combine-expr (operator operand expr)
 (cons (list operator operand (car expr)) (cdr expr))
)

(defun enclose-expression (expr)
  "Convert an expression in in-fix notation to Cambridge-Prefix
   notation."
  (check-type expr list)
  (if (null (rest expr)) expr
      (enclose-expression (combine-expr (second expr) (first expr) (rest (rest expr))))))

(defun enclose-term (expr)
  "Return expr with the first term collected as the first member and
   expressed in Cambridge Prefix notation."
  (check-type expr list)
  (if (or (string= "+" (symbol-name (second expr)))
	  (string= "-" (symbol-name (second expr))))
      expr
      (enclose-term (cons
		     (first (enclose-expression (list (first expr) (second expr) (third expr))))
		     (rest (rest (rest expr)))))))

(defun enclose-factor (expr)
  "Return expr with the first factor collected as the first member and
   expressed in Cambridge Prefix notation."
  (check-type expr list)
  (if (or (string= "*" (symbol-name (second expr)))
	  (string= "/" (symbol-name (second expr))))
      expr
      (enclose-factor (cons
		       (first (enclose-expression (list (first expr) (second expr) (third expr))))
		       (rest (rest (rest expr)))))))
