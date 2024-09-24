#lang racket

; O(n/2)のelement-of-setを使う

(define (element-of-set? x set)
	(cond ((null? set) false)
		((= x (car set)) true)
		((< x (car set)) false)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)))
