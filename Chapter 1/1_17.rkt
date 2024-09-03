#lang racket

(define (double x) (* x 2))
(define (halve x) (/ x 2))

; (define (mul a b)
; 	(if (= b 0)
; 		0
; 		(+ a (mul a (- b 1)))))

(define (mul a b)
	(cond	((= b 0) 0)
			((even? b) (mul (double a) (halve b)))
			(else (+ a (mul a (- b 1))))))

(define (even? n)
	(= (remainder n 2) 0))

(mul 10 100)