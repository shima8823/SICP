#lang racket

(define (filtered-accumulate filter combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter	(next a) 
					(combiner
						(if (filter a)
							(term a)
							null-value
						)
						result))
		)
	)
	(iter a null-value)
)

(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (identity x) x)
(define (prime? n) ; 1は素数になってしまう
	(define (smallest-divisor n) (find-divisor n 2))
	(define (find-divisor n test-divisor)
			(cond	((> (square test-divisor) n) n)
					((divides? test-divisor n) test-divisor)
					(else (find-divisor n (+ test-divisor 1)))
			)
	)
	(= n (smallest-divisor n))
)

; a
(define (sum-prime-square a b)
	(filtered-accumulate prime? + 0 square a inc b)
)

; b
(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))
	)
)

(define (pro-coprime n)
	(define (coprime? a)
		(= (gcd a n) 1)
	)

	(filtered-accumulate coprime? * 1 identity 1 inc n)
)

(sum-prime-square 2 5)
(pro-coprime 10)
