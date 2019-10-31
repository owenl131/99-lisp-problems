
;; Q41 - List of Goldbach compositions

;; Q46 - Truth tables for logical expressions
;; args: A, B, and(A,or(A,B))

(defun getVal (dict key)
  (cadar (remove-if (lambda (x) (not (eq key (car x)))) dict)))

(defun addVal (dict key value)
  (cons (list key value) dict))

(defun evalLogic (varsDict expr)
  (if (atom expr) 
    (getVal varsDict expr)
    (let ((op (car expr))
          (ex1 (evalLogic varsDict (cadr expr)))
          (ex2 (evalLogic varsDict (caddr expr))))
      (cond 
        ; currently does not support OR
        ((eq op 'and) (and ex1 ex2))
        ((eq op 'or) (or ex1 ex2))
        ((eq op 'nand) (not (and ex1 ex2)))
        ((eq op 'nor) (not (or ex1 ex2)))
        ((eq op 'xor) (not (eq ex1 ex2)))
        ((eq op 'impl) (not (and (not ex1) (not ex2))))
        ((eq op 'equ) (eq ex1 ex2))))))

(defun boolToWord (x)
  (if x "T" "F"))

(defun truthHelper (varsDict symList expr)
  (if (null symList)
    (let ((result (boolToWord (evalLogic varsDict expr)))
          (assignments 
            (reduce 
              (lambda (x y) (concatenate 'string y " " x)) 
              (map 'list 
                (lambda (x) (boolToWord (cadr x))) 
                varsDict)))) 
      (print (concatenate 'string assignments " | " result)))
    (progn
      (truthHelper (addVal varsDict (car symList) t) (cdr symList) expr)
      (truthHelper (addVal varsDict (car symList) nil) (cdr symList) expr))))

(defun truth (symList expr)
  (truthHelper '() symList expr))

(format t "~%Q46~%(AND A B)")
(truth '(A B) '(and A B))
(format t "~%(XOR A B)")
(truth '(A B) '(xor A B))
(format t "~%(XOR A B)")
(truth '(A B) '(xor A B))
(format t "~%(AND A (OR A B))")
(truth '(A B) '(and A (or A B)))
(format t "~%(AND A (AND B C))")
(truth '(A B C) '(and A (xor B C)))
(format t "~%")

;; Q48 - Truth tables for logical expressions (any number of variables)
;; args: [A, B, C], A and (B or C) equ A and B or A and C

;; Q49 - Gray code

;; Q50 - Huffman code

 