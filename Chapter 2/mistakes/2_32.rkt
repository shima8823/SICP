; 良問

#lang racket

(define nil `())

(define (subsets s)
	(if (null? s)
		(list nil)
		(let ((rest (subsets (cdr s))))
			(append
				rest
				(map
					(lambda (x)
						(cons (car s) x)
					)
					rest
				)
			)
		)
	)
)

#|

append (()) (map ?? (3))
append (() (3)) (map ?? (2 3))
append (() (3) (2) (2 3)) (map ?? (1 2 3))

|#

(subsets `(1 2 3))