(defpackage :bstree
  (:use :cl)
  (:export bstree bstreep insert root left
	   right member build-from-list inorder)
  (:shadow member))

(in-package :bstree)

(load "util.lisp")

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

(defun root (tree)
  "Return the root element of a tree"
  (check-type tree bstree)
  (if (atom tree) tree
      (first tree)))

(defun left (tree)
  "Return the left sub-tree of the tree."
  (check-type tree bstree)
  (if (atom tree) '()
      (second tree)))

(defun right (tree)
  "Return the right sub-tree of the tree."
  (check-type tree bstree)
  (if (atom tree) '()
      (third tree)))

(defun insert (elt tree)
  "Inserts an element ELT into a bst TREE."
  (check-type elt util:element)
  (check-type tree bstree)
  (cond ((null tree) elt)
	((eql (root tree) elt) tree)
	((string< elt (root tree))
	 (list (root tree)
	       (insert elt (left tree))
	       (right tree)))
	(t
	 (list (root tree)
	       (left tree)
	       (insert elt (right tree))))))

(defun member (elt tree)
  "Return True if ELT is a member of TREE."
  (check-type elt util:element)
  (check-type tree bstree)
  (cond ((null tree) nil)
	((string= elt (root tree)) t)
	((string< elt (root tree))
	 (member elt (left tree)))
	(t (member elt (right tree)))))

(defun build-from-list (elist)
  "Construct and return a bstree from a list ELIST."
  (check-type elist list)
  (if (null elist) '()
      (insert (first elist) (build-from-list (rest elist)))))

(defun inorder (bst)
  "Traverse a BST in-order and return the cons."
  (if (null bst) '()
      (append
       (inorder (left bst))
       (cons (root bst) (inorder (right bst))))))
