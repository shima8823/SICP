#lang racket

; 汎用
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
	(lambda (x)
		(f (g x))
	)
)


(define (repeated f n)
	(if (= n 0)
		(lambda (x) x)
		(compose (repeated f (- n 1)) f)
	)
)

((repeated square 2) 5)