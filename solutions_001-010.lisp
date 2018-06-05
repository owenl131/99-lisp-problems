
;; Q1 Find the last box of a list
(defun last-box (list)
  (if (equal (cdr list) '())
    (car list)
    (last-box (cdr list))))

(format t "~%Q1 - expected: ~%DORA")
(print (last-box '(alan barry candy dora)))

;; Q2 Find the last but one box of a list
(defun last-but-one (lst)
  (if (equal (cdr (cdr lst)) '())
    lst
    (last-but-one (cdr lst))))

(format t "~%Q2 - expected: ~%(CANDY DORA)")
(print (last-but-one '(alan barry candy dora)))

;; Q3 - find K'th element of a list (0-indexed)
(defun find-k (lst k)
  (if (equal k 0)
    (car lst)
    (find-k (cdr lst) (- k 1))))

(format t "~%Q3 - expected: ~%E")
(print (find-k '(a b c d e f g) 4))

;; Q4 Find the number of elements in a list
(defun list-len (lst)
  (if (equal lst '())
    0
    (+ 1 (list-len (cdr lst)))))

(format t "~%Q4 - expected: ~%5")
(print (list-len '(1 5 2 4 3)))

;; Q5 Reverse a list
(defun reverse-list (list)
  (if (equal list '())
    '()
    (append (reverse-list (cdr list)) (list (car list)))))

(format t "~%Q5 - expected: ~%(E D C B A)")
(print (reverse-list '(A B C D E)))

;; Q6 Find out whether a list is a palindrome
(defun is-palindrome (lst)
  (equal lst (reverse lst)))

(format t "~%Q6 - expected: ~%T~%NIL")
(print (is-palindrome '(1 2 3 2 1)))
(print (is-palindrome '(1 2 3 1 2)))

;; Q7 Flatten a nested list structure
(defun flatten-list (lst)
  (if (equal lst '())
    '()
    (if (listp (car lst))
      (append (flatten-list (car lst)) (flatten-list (cdr lst)))
      (append (list (car lst)) (flatten-list (cdr lst))))))

(format t "~%Q7 - expected: ~%(A B C D E F G)")
(print (flatten-list '(a b (c d) (e (f) () (g)))))

;; Q8 Eliminate consecutive duplicates in a list

;; Q9 Pack consecutive duplicates of list elements into sublists
"IN:  (pack '(a a a a b b c d d d))"
"OUT: ((A A A A) (B B) (C) (D D D))"

;; Q10 Run-length encoding of a list

(format t "~%")
