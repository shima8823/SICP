#lang racket

(define (expmod base exp m)
	(cond	((= exp 0) 1)
			((even? exp)
				(remainder
					(squaremod (expmod base (/ exp 2) m) m)
					m))
			(else
				(remainder
					(* base (expmod base (- exp 1) m))
					m))
	)
)

(define (squaremod x mod)
	(define y (remainder (square x) mod))

	; 1 (not (= x 1))
	; とも 
	; n − 1 (not (= x (- mod 1))) とも等しくない
	; 数値で、
	; その二乗が法 n に関して(define y (remainder (square x) mod))
	; 1 に等しい数値である (= y 1)
	
	(if (and (= y 1) (not (= x 1)) (not (= x (- mod 1)))) 
		0
		y)
)

(define (square x) (* x x))

(define (miller-rabin-test n)
	(define (try-it a)
		(= (expmod a n n) a)
	)
	(try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
	(cond	((= times 0) true)
			((miller-rabin-test n) (fast-prime? n (- times 1)))
			(else false)
	)
)

; (expmod 10 2 6)
(fast-prime? 3 100)

; カーマイケル数
(fast-prime? 561 100)
(fast-prime? 1105 100)
(fast-prime? 1729 100)
(fast-prime? 2465 100)
(fast-prime? 2821 100)
(fast-prime? 6601 100)

