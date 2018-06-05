
;; Q11 -  Run-length encoding,
;; but elements without duplicates are left alone


;; Q12 - Run-length decoding

;; Q13 - Direct run-length encoding

;; Q14 - Duplicate elements of a list
(defun dup-elem (lst)
  (if (equal lst '()) 
  	'()
  	(cons (car lst) (cons (car lst) (dup-elem (cdr lst))))))

(format t "~%Q14 - expected: ~%(A A B B C C C C D D)")
(print (dup-elem '(a b c c d)))

;; Q15 - Replicate n-times

;; Q16 - Drop every n'th element

;; Q17 - Split list into 2 parts based on argument

;; Q18 - Extract slice from list

;; Q19 - Rotate list by N places

;; Q20 - Remove k'th element from list
