#lang sicp
(#%require "./stream_basic_components.rkt")

(define (average x y) (/ (+ x y) 2.0))
(define (square x) (* x x))
(define (abs x) (if (< x 0) (* x -1) x))

(define (sqrt-improve guess x)
	(average guess (/ x guess)))

(define (sqrt-stream x)
	(define guesses
		(cons-stream
			1.0
			(stream-map
				(lambda (guess) (sqrt-improve guess x))
				guesses)))
	guesses)

(define (stream-limit stream tolerance)
	(if (<= (abs (- (stream-car stream) (stream-car (stream-cdr stream)))) tolerance)
		(stream-car (stream-cdr stream))
		(stream-limit (stream-cdr stream) tolerance)))

(define (sqrt x tolerance)
	(stream-limit (sqrt-stream x) tolerance))

(sqrt 2.0 0.001)

