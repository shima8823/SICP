#lang racket

(define (right-split painter n)
	(if (= n 0)
		painter
		(let
			((smaller (right-split painter (- n 1))))
			(beside painter (below smaller smaller))
		)
	)
)

(define (up-split painter n)
	(if (= n 0)
		painter
		(let
			((smaller (up-split painter (- n 1))))
			(below painter (beside smaller smaller))
		)
	)
)


(define (split f1 f2)
	(lambda (painter n)
		(if (= n 0)
			painter
			(let
				((smaller ((split f1 f2) painter (- n 1))))
				(f2 painter (f1 smaller smaller))
			)
		)
	)
)
