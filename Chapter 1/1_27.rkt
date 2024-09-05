#lang racket

(define (expmod base exp m)
	(cond ((= exp 0) 1)
	((even? exp)
		(remainder
			(square (expmod base (/ exp 2) m))
			m))
	(else
		(remainder
			(* base (expmod base (- exp 1) m))
			m))
	)
)

(define (square x) (* x x))

; 2465(5 * 493)
(define (fermat-test n)
	(define (try-it a)
		(cond	((= a 0) true)
				((= (expmod a n n) a) (try-it (- a 1)))
				(else false)
		)
	)	
	(try-it (- n 1))
)

(define (complete-fast-prime? n)
	(fermat-test n)
)

(complete-fast-prime? 2)
(complete-fast-prime? 13)
(complete-fast-prime? 14)

(complete-fast-prime? 561)
(complete-fast-prime? 1105)
(complete-fast-prime? 1729)
(complete-fast-prime? 2465)
(complete-fast-prime? 2821)
(complete-fast-prime? 6601)
; 561, 1105, 1729, 2465, 2821, 6601