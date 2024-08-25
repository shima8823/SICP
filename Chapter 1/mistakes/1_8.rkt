#lang racket

(define (abs x)
	(cond ((< x 0) (- x))
		(else x)))

(define (improve guess x)
	 (/ (+ (/ x guess guess) guess guess) 3))

(define (good-enough? guess x)
	(< (abs (- (* guess guess guess) x)) 0.001))

(define (cubt-iter guess x) 
	(if (good-enough? guess x)
		guess
		(cubt-iter (improve guess x) x)))

(define (cubt x)
	(cubt-iter 1.0 x))

(cubt 8)