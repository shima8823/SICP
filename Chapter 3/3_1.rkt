#lang racket

(define (make-accumulator value)
	(lambda (add-v) 
		(begin (set! value (+ value add-v))
				value)))

(define A (make-accumulator 5))
(A 10)
; 15
(A 10)
; 25