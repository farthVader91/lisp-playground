(defpackage match
 (:use "COMMON-LISP" "COMMON-LISP-USER")
 (:export :variablep :match-element :dont-care :boundp)
 (:shadow :boundp)
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

(defun matchlelt (l1 l2)
	"Returns T if all elements of L1 are equal to
         all elements of L2, with the exception of #\?
         which is treated as a wild-character."
	(check-type l1 list)
	(check-type l2 list)
	(cond ((null l1) (null l2))
	      ((null l2) nil)
	      ((or (match:dont-care (first l1))
		   (match:dont-care (first l2))
		   (eql (first l1) (first l2)))
	       (matchlelt (rest l1) (rest l2)))
	      (t nil)))

(defun boundp (v subs)
  "Return True if variable V is bound to anything in substitution S."
  (check-type subs list)
  (check-type v (satisfies match:variablep))
  (and (assoc v subs) t))
