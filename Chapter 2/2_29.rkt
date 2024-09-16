#lang racket

#|

## d
right-branch
branch-structure
を変えてあげれば良い

|#

(define (make-mobile left right)
	(list left right)
	; (cons left right)
)

(define (make-branch length structure)
	(list length structure)
	; (cons length structure)
)

; ## a
; selector
(define (left-branch mobile)
	(car mobile)
)
(define (right-branch mobile)
	(car (cdr mobile))
	; (cdr mobile) ; list -> cons
)

(define (branch-length branch)
	(car branch)
)
(define (branch-structure branch)
	(car (cdr branch))
	; (cdr branch) ; list -> cons
)
; ##

; b
(define (total-weight mobile)
	(if (pair? mobile) 
		(+ (total-weight (branch-structure (left-branch mobile)))
			(total-weight (branch-structure (right-branch mobile)))
		)
		mobile
	)
)
; ##

; c
(define (balanced? mobile)
	(let
		(
			(l-mobile (left-branch mobile))
			(r-mobile (right-branch mobile))
		)
		(if (not (pair? l-mobile))
			#t
			(let
				(
					(l-length (branch-length l-mobile))
					(r-length (branch-length r-mobile))
					(l-weight (total-weight (branch-structure l-mobile)))
					(r-weight (total-weight (branch-structure r-mobile)))
				)
				(let
					(
						(l-mul (* l-length l-weight))
						(r-mul (* r-length r-weight))
					)
					(and (= l-mul r-mul)
						(balanced? l-mobile)
						(balanced? r-mobile)
					)
				)
			)
		)
	)
)
; ##

(define x '((6 ((1 2) (2 1))) (3 ((2 4) (4 2)))))
(define x-self
	(make-mobile 
		(make-branch 6 (make-mobile (make-branch 1 2) (make-branch 2 1)))
		(make-branch 3 (make-mobile (make-branch 2 4) (make-branch 4 2)))
	)
)
(define notbalanced '((3 ((1 2) (2 1))) (3 ((2 4) (4 2)))))

(total-weight x)
(balanced? x)
(balanced? notbalanced)
x-self
(total-weight x-self)
(balanced? x-self)

(left-branch x-self)
(right-branch x-self)
