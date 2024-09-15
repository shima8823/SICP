#lang racket

(define nil `())

(define (for-each proc items)
	(let
		(
			(proc-items (map proc items))
		)
		#t
	)
)


(for-each
	(lambda (x)
		(newline)
		(display x)
	)
	(list 57 321 88)
)