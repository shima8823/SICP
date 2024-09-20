#lang racket

(define (flip-horiz paitner)
	(transform-painter painter
		(make-vect 1.0 0.0)
		(make-vect 0.0 0.0)
		(make-vect 1.0 1.0))
)

(define (rotate180 paitner)
	(transform-painter painter
		(make-vect 1.0 1.0)
		(make-vect 0.0 1.0)
		(make-vect 1.0 0.0))
)

(define (rotate270 paitner)
	(transform-painter painter
		(make-vect 0.0 1.0)
		(make-vect 0.0 0.0)
		(make-vect 1.0 1.0))
)