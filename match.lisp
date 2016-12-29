(defpackage match
 (:use "COMMON-LISP" "COMMON-LISP-USER")
 (:export :variablep :match-element :dont-care :boundp :bound-to :match)
 (:shadow :boundp :substitute)
)
(in-package "MATCH")

(defun variablep (s)
  (and (symbolp s) (char= (char (symbol-name s) 0) #\?)))

(defun dont-care (e)
    (if (symbolp e) (string= (symbol-name e) #\?) nil))



(defun match-element (e1 e2)
 (cond ((and (not (dont-care e1)) (variablep e1))
            (list e1 e2))
       ((and (not (dont-care e2)) (variablep e2))
            (list e2 e1))
       (t
            (or (eql e1 e2) (or (dont-care e1) (dont-care e2))))))

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

(defun bound-to (v subs)
  "Returns the term that the variable V is bound to in substitution S."
  (check-type subs list)
  (check-type v (satisfies match:variablep))
  (second (assoc v subs)))

(defun match1 (pat lst pairs)
  (cond ((or (null pat) (null lst))
	 (cons '(t t) pairs))
	((variablep (first pat))
	 (cond ((eql (match:bound-to (first pat) pairs) (first lst))
		(match1
		 (rest pat)
		 (rest lst)
		 pairs))		
	       ((eql (match:bound-to (first pat) pairs) nil)
		(match1
		 (rest pat)
		 (rest lst)
		 (cons (list (first pat) (first lst)) pairs)))))
	((eql (first pat) (first lst))
	 (match1 (rest pat) (rest lst) pairs))))

(defun match (pat lst)
  "Return a substitution - a list of all pairs (V A) where V is a
   variable in pat and A is the corresponding element in lst. If the
   nth member of pat is not a variable, it must be eql to the nth
   member of lst. Otherwise, match should return NIL.  If no element
   of pat is a variable but each is eql to its corresponding element
   of lst, match should return ((T T)). If a variable occurs more than
   once in pat, its corresponding elements in lst must be the same."
  (check-type pat list)
  (check-type lst list)
  (match1 pat lst '()))

(defun substitute (pat subs)
  "Substitute every occurence of variable V in pat, with it's
   corresponding occurrence in substition subs."
  (check-type pat list)
  (check-type subs list)
  (if (null pat) '()
      (cons (match:bound-to (first pat) subs)
	    (substitute (rest pat) subs))))
