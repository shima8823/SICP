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

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a)
	)
	(try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
	(cond	((= times 0) true)
			((fermat-test n) (fast-prime? n (- times 1)))
			(else false)
	)
)

; (expmod 10 2 6)
(fast-prime? 5 100)

; カーマイケル数
(fast-prime? 561 100)
(fast-prime? 1105 100)
(fast-prime? 1729 100)
(fast-prime? 2465 100)
(fast-prime? 2821 100)
(fast-prime? 6601 100)

