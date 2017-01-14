(in-package :cl)

(load "util.lisp")

(defpackage set
	(:use :cl :util)
	(:export setp set makeset union first rest insert intersection complement subsetp equal empty)
	(:shadow set union first rest insert intersection complement subsetp equal))

(in-package :set)

(defun setp (l)
	"Return True if L is a list of distinct objects,
         False otherwise."
	(cond ((not (listp l)) nil)
	      ((string= (cl:first l) :set))))


(deftype set ()
         "Set object that satisfies setp"
	 '(satisfies setp))

(defun unlabelled-makeset (b)
	(cond ((null b) '())
	      ((member (cl:first b) (cl:rest b))
	       (unlabelled-makeset (cl:rest b)))
	      (t (cons (cl:first b) (unlabelled-makeset (cl:rest b))))))

(defun makeset (b)
        "Return an unordered collection of unique elements,
         given a bag of elements."
        (check-type b util:bag)
	(cons :set (unlabelled-makeset b)))

(defun union-unlabelled-sets (s1 s2)
	(cond ((null s1) s2)
	      ((member (cl:first s1) s2)
	       (union-unlabelled-sets (cl:rest s1) s2))
	      (t (cons (cl:first s1) (union-unlabelled-sets (cl:rest s1) s2)))))

(defun union (s1 s2)
        "Return the union of 2 sets s1 and s2."
	(check-type s1 set)
	(check-type s2 set)
	(cons :set (union-unlabelled-sets (cl:rest s1) (cl:rest s2))))

(defun first (s)
        "Return the first element listed in a set S."
	(check-type s set)
	(cl:first (cl:rest s)))

(defun rest (s)
        "Return the rest of the set S without the first element."
	(check-type s set)
	(cons :set (cl:rest (cl:rest s))))

(defun insert (e s)
        "Insert an element E into S if it already doesn't exist in S.
         Finally return S."
	(check-type s set)
        (if (member e s) s
	    (cons e s)))

(defun empty (s)
        "Return True if set S is empty, False otherwise."
	(check-type s set)
	(null (cl:rest s)))

(defun intersection-unlabelled (s1 s2)
	(cond ((null s1) '())
	      ((member (cl:first s1) s2)
	       (cons
		(cl:first s1)
		(intersection-unlabelled (cl:rest s1) s2)))
	      (t (intersection-unlabelled (cl:rest s1) s2))))

(defun intersection (s1 s2)
        "Return set of elements that are common across sets S1 and S2."
	(check-type s1 set)
	(check-type s2 set)
	(cons :set (intersection-unlabelled (cl:rest s1) (cl:rest s2))))

(defun complement-unlabelled (s1 s2)
        (cond ((null s1) '())
	      ((member (cl:first s1) s2)
	       (complement-unlabelled (cl:rest s1) s2))
	      (t (cons (cl:first s1)
		       (complement-unlabelled (cl:rest s1) s2)))))

(defun complement (s1 s2)
        "Return set of disjoint elements across sets S1 and S2."
	(check-type s1 set)
	(check-type s2 set)
	(cons :set (complement-unlabelled (cl:rest s1) (cl:rest s2))))

(defun subsetp-unlabelled (s1 s2)
	(cond ((null s1) t)
	      ((member (cl:first s1) s2) (subsetp-unlabelled (cl:rest s1) s2))
	      (t nil)))

(defun subsetp (s1 s2)
        "Return True if all elements of set S1 are members of set S2,
         False otherwise."
	(check-type s1 set)
	(check-type s2 set)
	(subsetp-unlabelled (cl:rest s1) (cl:rest s2)))

(defun equal (s1 s2)
        "Return True if all elements of set S1 are members of set S2,
         and vice-versa. Return False otherwise."
	(check-type s1 set)
	(check-type s2 set)
	(and (subsetp s1 s2) (subsetp s2 s1)))
