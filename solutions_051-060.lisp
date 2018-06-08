
;; Q54 - Check if argument is a valid binary tree
(defun is-tree (tree)
  (cond 
    ((not tree) t)
    ((not (listp tree)) nil)
    ((atom (car tree))
     (and 
      (not (equal (cdr (cdr tree)) '())) 
      (is-tree (car (cdr tree)))
      (is-tree (car (cdr (cdr tree))))))
    (t nil)))

(format t "~%Q54 - expected: ~%T T T T NIL NIL")
(print (is-tree nil))
(print (is-tree '(a nil nil)))
(print (is-tree '(a (b nil nil) nil)))
(print (is-tree '(a (b (a nil nil) (c nil nil) nil) (d nil nil))))
(print (is-tree '(a (b nil nil))))
(print (is-tree '(a (b nil nil) c)))

;; Q55 - Construct completely balanced binary trees
;;       Left and right subtree differ by at most 1 node
(defun cb-tree (nodes)
  (cond 
    ((equal nodes 0) nil)
    ((equal nodes 1) '(x nil nil))
    ((equal (mod nodes 2) 0)
      '(x (cb-tree (- (/ nodes 2) 1)) (cb-tree (/ nodes 2))))
    (t '(x (cb-tree (/ nodes 2) (/ nodes 2))))))
; not done yet

;; Q56 - Check if 2 trees are Symmetric binary trees

;; Q57 - Construct binary search tree from list of integers

;; Q58 - Generate and test paradigm -> construct all symmetric,
;; completely balanced binary trees with a given number of nodes

;; Q59 - Construct height-balanced binary trees

;; Q60 - Construct height-balanced binary tree with given number of nodes

(format t "~%")
