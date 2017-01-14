(defpackage util
	(:use :cl)
	(:export elementp element bag)
)

(in-package :util)

(defun elementp (ele)
	(or (symbolp ele) (characterp ele) (numberp ele) (packagep ele))
)

(deftype element ()
	"Elements are objects testable by EQL,
         namely symbols, characters, numbers and packages."
	'(satisfies util:elementp))

(deftype bag ()
        "Unordered collection of elements that can be repeated."
        '(satisfies listp))
