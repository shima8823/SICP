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

(define (horner-eval x coefficient-sequence)
	(accumulate
		(lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
		0
		coefficient-sequence
	)
)

#|

32 + 0 + 40 + 0 + 6 + 1 = 79


|#

(horner-eval 2 (list 1 3 0 5 0 1))