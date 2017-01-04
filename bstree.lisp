(defpackage :bstree
  (:use :cl :util)
  (:export :bstree :bstreep :bstree-insert
	   :bstree-root :bstree-left :bstree-right))

(in-package :bstree)

(deftype bstree ()
  "Define the Binary Search Tree type."
  '(satisfies bstreep))

(defun bstreep(tree)
  "A bstree is either an element, or a three-member list,
   the first of which is an element."
  (or (typep tree 'util:element)
      (and (listp tree)
	   (= (length tree) 3)
	   (typep (first tree) 'util:element))))

(defun bstree-insert (elt tree)
  "Inserts an element ELT into a bst TREE."
  (check-type elt util:element)
  (check-type tree bstree)
  (cond ((null tree) elt)
	((eql (bstree-root tree) elt) tree)
	((string< elt (bstree-root tree))
	 (list (bstree-root tree)
	       (bstree-insert elt (bstree-left tree))
	       (bstree-right tree)))
	(t
	 (list (bstree-root tree)
	       (bstree-left tree)
	       (bstree-insert elt (bstree-right tree))))))

(defun bstree-root (tree)
  "Return the root element of a tree"
  (check-type tree bstree)
  (if (atom tree) tree
      (first tree)))

(defun bstree-left (tree)
  "Return the left sub-tree of the tree."
  (check-type tree bstree)
  (if (atom tree) '()
      (second tree)))

(defun bstree-right (tree)
  "Return the right sub-tree of the tree."
  (check-type tree bstree)
  (if (atom tree) '()
      (third tree)))
