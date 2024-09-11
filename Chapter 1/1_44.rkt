#lang racket

(define dx 0.1)
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (cubic a b c)
	(lambda (x) (+ (cube x) (* a (square x)) (* b x) c))
)

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

(define (smooth f)
	(lambda (x)
		(/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)
	)
)

; ((smooth (cubic 3 3 1)) 1)
(((repeated smooth 2) cube) 3)
