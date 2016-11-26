(defpackage util
	(:use :common-lisp)
	(:export :elementp)
)

(in-package :util)

(defun elementp (element)
	(or (symbolp element) (characterp element) (numberp element) (packagep element))
)