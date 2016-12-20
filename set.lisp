(in-package :cl)

(load "util.lisp")

(defpackage set
	(:use :cl :util)
	(:export setp set makeset union)
	(:shadow set union))

(in-package :set)

(defun setp (l)
	"Return True if L is a list of distinct objects,
         False otherwise."
	(cond ((not (listp l)) nil)
              ((null l) T)
	      ((member (first l) (rest l)) nil)
	      (t (setp (rest l)))))

(deftype set ()
         "Set object that satisfies setp"
	 '(satisfies setp))

(defun unlabelled-makeset (b)
	(cond ((null b) '())
	      ((member (first b) (rest b))
	       (unlabelled-makeset (rest b)))
	      (t (cons (first b) (unlabelled-makeset (rest b))))))

(defun makeset (b)
        "Return an unordered collection of unique elements,
         given a bag of elements."
        (check-type b util:bag)
	(cons :set (unlabelled-makeset b)))

(defun union (s1 s2)
        "Return the union of 2 sets s1 and s2."
	(check-type s1 set)
	(check-type s2 set)
	(cond ((null s1) s2)
	      ((member (first s1) s2)
	       (union (rest s1) s2))
	      (t (cons (first s1) (union (rest s1) s2)))))
