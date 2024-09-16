#lang racket

(define (make-mobile left right)
	(list left right))

(define (make-branch length structure)
	(list length structure))

; ## a
; selector
(define (left-branch mobile)
	(car mobile)
)
(define (right-branch mobile)
	(car (cdr mobile))
)

(define (branch-length branch)
	(car branch)
)
(define (branch-structure branch)
	(car (cdr branch))
)
; ##

(define (total-weight mobile)
	(if (pair? mobile) 
		(+ (total-weight (branch-structure (left-branch mobile)))
			(total-weight (branch-structure (right-branch mobile)))
		)
		mobile
	)
)

(define x '((6 ((1 2) (2 1))) (3 ((2 4) (4 2)))))

(total-weight x)
