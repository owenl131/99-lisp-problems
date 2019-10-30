
(defun find-k (lst k)
  (if (equal k 0)
    (car lst)
    (find-k (cdr lst) (- k 1))))
(defun split-2 (lst n)
  (cond 
    ((equal lst '()) (list '() '()))
    ((equal n 0) (list '() lst))
    (t (let ((res (split-2 (cdr lst) (- n 1))))
      (list (cons (car lst) (car res)) (car (cdr res)))))))
(defun slice (lst a b)
  (car (split-2 (car (cdr (split-2 lst a))) (- b a))))

;; Q21 - Insert element at given position
(defun insert (lst value pos)
  (let ((split (split-2 lst pos)))
    (append (car split) (list value) (car (cdr split)))))

(format t "~%Q21 - expected: ~%(A B C D E \"HELLO\" F G)")
(print (insert '(a b c d e f g) "HELLO" 5))

;; Q22 - Range list
(defun range (start end)
  (if (equal start end) '()
    (cons start (range (+ start 1) end))))

(format t "~%Q22 - expected: ~%(3 4 5 6)")
(print (range 3 7))

;; Q23 - Extract randomly selected elements from list

;; Q24 - Draw N different random numbers from 1..M
(defun rand-set (nums range)
  (if (equal nums 0) '()
    (let ((rest (rand-set (- nums 1) range)))
      (loop 
        (let ((x (random range)))
          (cond 
            (
              (notany (lambda (y) (equal x y)) rest)
              (setq rest (cons x rest))
              (return)
            ))))
      rest)))

(format t "~%Q24 - expected: ~%Random length 5 array (no duplicates)")
(print (rand-set 5 10))
(print (rand-set 5 10))
(print (rand-set 5 10))
(print (rand-set 10 10))

;; Q25 - Generate a random permutation of a list
(defun cut-deck (lst len)
  (if (equal len 0) lst 
    (let ((x (random len)))
      (let ((split (split-2 lst x)))
        (append 
          (car (cdr split))
          (car split))))))
(defun shuffle-once (lst len)
  (let ((x (random len)))
    (let ((split (split-2 lst x)))
      (append 
        (cut-deck (car (cdr split)) (- len x))
        (cut-deck (car split) x)))))
(defun shuffle-n (lst len n)
  (if (equal n 0) lst 
    (shuffle-n (shuffle-once lst len) len (- n 1))))
(defun shuffle (lst)
  (let 
    ((len (length lst)))
    (shuffle-n lst len 100)))

(format t "~%Q25 - expected: shuffled")
(print (shuffle '(a b c d e f g)))
(print (shuffle '(a b c d e f g)))
(print (shuffle '(a b c d e f g)))
(print (shuffle '(a b c d e f g)))

;; count how many shuffles before returning to first state
(defun validate-shuffle (lst)
  (let 
    ((cpy (copy-list lst))
     (ctr 0))
    (loop 
      (setq cpy (shuffle cpy))
      (setq ctr (+ ctr 1))
      (if (equal lst cpy) (return)))
    ctr))

(format t "~%Iterations before meeting initial state, length 7:")
(print (validate-shuffle '(a b c d e f g)))


;; Q26 - Generate combinations of K objects from N elements
(defun choose-k (lst k)
  (let 
    ((len (length lst)))
    (let 
      ((sel (slice (shuffle (range 0 len)) 0 k)))
      (map 'list (lambda (x) (find-k lst x)) sel))))

(format t "~%Q26: expected to choose 5 items at random from (a b c d e f g):")
(print (choose-k '(a b c d e f g) 5)) 
(print (choose-k '(a b c d e f g) 5)) 
(print (choose-k '(a b c d e f g) 5)) 

;; Q27 - Group elements of set into disjoint subsets

;; Q28 - Sort list based on length of sublists

(format t "~%")
