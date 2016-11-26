(defpackage match
 (:use "COMMON-LISP" "COMMON-LISP-USER")
 (:export :variablep :match-element)
)
(in-package "MATCH")

(defun variablep (s)
  (and (symbolp s) (char= (char (symbol-name s) 0) #\?))
)

(defun match-element (e1 e2)
 (or (eql e1 e2) (or (variablep e1) (variablep e2)))
)
