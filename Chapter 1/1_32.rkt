#lang racket

(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a)
			(accumulate combiner null-value term (next a) next b)
		)
	)
)

(define (integral f a b dx)
	(define (add-dx x)
		(+ x dx))
	(* (accumulate + 0 f (+ a (/ dx 2.0)) add-dx b)
		dx)
)

(define (factorial n)
	(define (identity x) x)
	(define (inc x) (+ x 1))
	(accumulate * 1 identity 1 inc n)
)

(define (cube x) (* x x x))

(integral cube 0 1 0.01) ; 1/4
(factorial 5) ; 120

; b
(define (acc-iter combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner (term a) result))
		)
	)
	(iter a null-value)
)

(define (inte-iter f a b dx)
	(define (add-dx x)
		(+ x dx))
	(* (acc-iter + 0 f (+ a (/ dx 2.0)) add-dx b)
		dx)
)

(define (fact-iter n)
	(define (identity x) x)
	(define (inc x) (+ x 1))
	(acc-iter * 1 identity 1 inc n)
)

(inte-iter cube 0 1 0.01) ; 1/4
(fact-iter 5) ; 120
