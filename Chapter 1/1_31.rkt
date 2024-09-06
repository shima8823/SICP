#lang racket

; a 再帰
(define (product term a next b)
	(if (> a b)
		1
		(* (term a)
			(product term (next a) next b)
		)
	)
)

(define (factorial n)
	(define (identity x) x)
	(define (inc x) (+ x 1))
	(product identity 1 inc n)
)

(factorial 5)


(define (pi-pro n)
	(define (term-top n) (* (* 2 n) (* 2 n)))
	(define (term-bottom n) (* (- (* 2 n) 1) (+ (* 2 n) 1)))
	(define (term n) (/ (term-top n) (term-bottom n)))
	(define (inc x) (+ x 1))

	(product term 1.0 inc n)
)

(* 2 (pi-pro 1000))
; 3.1415925750356295

; b 反復
(define (product-iter term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* (term a) result))
		)
	)
	(iter a 1)
)

(define (factorial-iter n)
	(define (identity x) x)
	(define (inc x) (+ x 1))
	(product-iter identity 1 inc n)
)

(factorial-iter 5)