#lang racket

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (mul a b) (fast-mul-iter a b 0))

(define (fast-mul-iter a b n)
	(cond	((= b 0) n)
			((even? b) (fast-mul-iter (double a) (halve b) n))
			(else (fast-mul-iter a (- b 1) (+ n a)))))

(define (even? n)
	(= (remainder n 2) 0))

(mul 31 33)