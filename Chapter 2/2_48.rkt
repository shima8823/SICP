#lang racket

(define (make-segment start end)
	(cons start end)
)

(define (start-segment seg)
	(car seg)
)

(define (end-segment)
	(cdr seg)
)

