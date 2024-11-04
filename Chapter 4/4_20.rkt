#lang sicp

(define (f x)
	(letrec
		((even? (lambda (n)	(if (= n 0) true (odd? (- n 1)))))
		 (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
	(even? x)))

; (f 10)
(letrec
	((fact
		(lambda (n)
			(if (= n 1) 1 (* n (fact (- n 1)))))))
	(fact 10))