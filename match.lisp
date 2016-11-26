(defpackage match
 (:use "COMMON-LISP" "COMMON-LISP-USER")
 (:export :variablep :match-element :dont-care)
)
(in-package "MATCH")

(defun variablep (s)
  (and (symbolp s) (char= (char (symbol-name s) 0) #\?))
)

(defun match-element (e1 e2)
 (cond ((and (not (dont-care e1)) (variablep e1))
            (list e1 e2))
       ((and (not (dont-care e2)) (variablep e2))
            (list e2 e1))
       (t
            (or (eql e1 e2) (or (dont-care e1) (dont-care e2))))
 )
)

(defun dont-care (e)
    (if (symbolp e) (string= (symbol-name e) #\?) nil)
)