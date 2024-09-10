#lang racket

(define (inc x) (+ x 1)
)
(define (double f)
	(lambda (x) (f (f x)))

)

((double inc) 1)
(((double (double double)) inc) 5)

#|

(
	(
		(double
			(double double)
		) 
		inc
	)
	5
)

(2^2) ^ (2^2) = 16



|#
(
	(
		(lambda (x) (
			(lambda (x) (double (double x)))
			((lambda (x) (double (double x))) x))
		)
		inc
	)
	5
)