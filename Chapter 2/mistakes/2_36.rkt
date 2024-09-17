; 良問 2_32の部分集合みたい

#lang racket

(define nil `())

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence))
		)
	)
)

(define (accumulate-n op init seqs)
	(display seqs)
	(newline)
	(if (null? (car seqs))
		nil
		(cons
			(accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs))
		)
	)
)

(define x `((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate + 0 `(1 2 3))
(accumulate-n + 0 `((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

; (car `(()))
; (cons 2 `(4 5 6))
; (list (cdr x))
; (car (list (cdr x)))