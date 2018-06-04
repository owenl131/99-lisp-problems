;; Q1 Find the last box of a list
(defun last-box (list)
  (if (equal (cdr list) '())
    (car list)
    (last-box (cdr list))))

"Q1 - expected: DORA"
(last-box '(alan barry candy dora))

;; Q2 Find the last but one box of a list
(defun last-but-one (list)
  (if (equal (cdr (cdr list)) '())
    list
    (last-but-one (cdr list))))

"Q2 - expected: (CANDY DORA)"
(last-but-one '(alan barry candy dora))

;; Q3 - find K'th element of a list (0-indexed)
(defun find-k (lst k)
  (if (equal k 0)
    (car lst)
    (find-k (cdr lst) (- k 1))))

"Q3 - expected: E"
(find-k '(a b c d e f g) 4)

