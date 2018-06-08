
;; Q31 - Determine if number is prime


;; Q32 - Determine GCD of two positive integers

;; Q33 - Determine if two positive integers are coprime

;; Q34 - Calculate Euler's Totient function

;; Q35 - Find prime factors
(defun find-factors (num)
  (if (equal num 1) '()
    (let ((fact 2))
      (loop 
        (if (equal (mod num fact) 0)
          (return (cons fact (find-factors (/ num fact))))
          (setq fact (+ fact 1)))))))

(format t "~%Q35 - expected: ~%(2 2 3 5 11)")
(print (find-factors 660))
(format t "~%(7907 7919)")
(print (find-factors 62615533))

;; Q36 - Find prime factors (grouped)

;; Q37 - Calculate Euler's Totient function (improved)

;; Q38 - Compare the two methods of calculating Euler's totient

;; Q39 - Construct a list of primes numbers between A and B

;; Q40 - Goldbach's conjecture

(format t "~%")
