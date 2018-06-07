
;; Q11 -  Run-length encoding,
;; but elements without duplicates are left alone


;; Q12 - Run-length decoding
(defun copy-times (value times)
  (if (equal times 0)
    '()
    (cons value (copy-times value (- times 1)))))

(defun de-run-length (lst)
  (if (equal lst '())
    '()
    (append 
      (copy-times 
        (car (car lst))
        (car (cdr (car lst))))
      (de-run-length (cdr lst)))))

(format t "~%Q12 - expected: ~%(A A A B B C D D D D A)")
(print (de-run-length '((a 3) (b 2) (c 1) (d 4) (a 1))))

;; Q13 - Direct run-length encoding

;; Q14 - Duplicate elements of a list
(defun dup-elem (lst)
  (if (equal lst '()) 
    '()
    (cons (car lst) (cons (car lst) (dup-elem (cdr lst))))))

(format t "~%Q14 - expected: ~%(A A B B C C C C D D)")
(print (dup-elem '(a b c c d)))

;; Q15 - Replicate n-times
(defun replicate-n (lst n)
  (cond 
    ((equal n 1) lst)
    ((equal lst '()) '())
    (t (append (copy-times (car lst) n) (replicate-n (cdr lst) n)))))

(format t "~%Q15 - expected: ~%(A A A A A A B B B)")
(print (replicate-n '(a a b) 3))

;; Q16 - Drop every n'th element
(defun skip (lst n)
  (if (equal n 0) lst (skip (cdr lst) (- n 1))))

;; Q17 - Split list into 2 parts based on argument
(defun split-2 (lst n)
  (cond 
    ((equal lst '()) (list '() '()))
    ((equal n 0) (list '() lst))
    (t (let ((res (split-2 (cdr lst) (- n 1))))
      (list (cons (car lst) (car res)) (car (cdr res)))))))
(format t "~%Q17 - expected: ~%((A B C D) (E F G))")
(print (split-2 '(a b c d e f g) 4))

;; Q18 - Extract slice from list
(defun slice (lst a b)
  (car (split-2 (car (cdr (split-2 lst a))) (- b a))))

(format t "~%Q18 - expected: ~%(D E F)~%(A B C D)")
(print (slice '(a b c d e f g) 3 6))
(print (slice '(a b c d e f g) 0 4))

;; Q19 - Rotate list by N places


;; Q20 - Remove element at index k from list
(defun remove-k (lst k)
  (append (slice lst 0 k) (slice lst (+ k 1) 100000)))

(format t "~%Q20 - expected: ~%(A B C D F G)")
(print (remove-k '(a b c d e f g) 4))

(format t "~%")
