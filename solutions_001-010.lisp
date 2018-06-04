;; Q1 Find the last box of a list
(defun last-box (list)
  (if (equal (cdr list) '())
    (car list)
    (last-box (cdr list))))

(print "Q1 - expected: DORA")
(last-box '(alan barry candy dora))

;; Q2 Find the last but one box of a list
(defun last-but-one (list)
  (if (equal (cdr (cdr list)) '())
    list
    (last-but-one (cdr list))))

(print "Q2 - expected: (CANDY DORA)")
(last-but-one '(alan barry candy dora))



