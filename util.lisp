(defpackage util
	(:use :common-lisp)
	(:export elementp  element bag)
)

(in-package :util)

(defun elementp (element)
	(or (symbolp element) (characterp element) (numberp element) (packagep element))
)

(deftype element ()
	"Elements are objects testable by EQL,
         namely symbols, characters, numbers and packages."
	'(satisifies util:elementp))

(deftype bag ()
        "Unordered collection of elements that can be repeated."
        '(satisfies listp))
