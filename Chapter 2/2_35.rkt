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


(define (count-leaves x)
	(cond	((null? x) 0)
			((not (pair? x)) 1)
			(else (+
					(count-leaves (car x))
					(count-leaves (cdr x))
				)
			)
	)
)

(define (count-leaves-acc t)
	(accumulate
		+
		0
		(map
			(lambda (sub-tree)
				(if (pair? sub-tree)
					(count-leaves-acc sub-tree)
					1
				)
			)
			t
		)
	)
)

(define x (cons (list 1 2) (list 3 4)))
(count-leaves (list x x))
(count-leaves-acc (list x x))
(count-leaves x)
(count-leaves-acc x)