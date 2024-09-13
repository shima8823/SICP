#lang racket

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (max (car interval) (cdr interval)))
(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (add-interval x y)
	(make-interval
		(+ (lower-bound x) (lower-bound y))
		(+ (upper-bound x) (upper-bound y))
	)
)

(define (sub-interval x y)
	(make-interval
		(- (lower-bound x) (upper-bound y))
		(- (upper-bound x) (lower-bound y))
	)
)

(define (mul-interval x y)
	(let (
		(p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y)))
		)
		(make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
	)
)

(define (div-interval x y)
	(if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
		(error "Values cross 0")
		(mul-interval
			x
			(make-interval
				(/ 1.0 (upper-bound y))
				(/ 1.0 (lower-bound y))
			)
		)
	)
)

(let
	(
		(x (make-interval -1 14))
		; y-reverse: (1/8 . -1.0)
		(y (make-interval -1 8))
		; 正解: -14 -1/8
		; 実際: -14 14/8
		(
			y-reverse
			(make-interval
				(/ 1.0 (upper-bound (make-interval -1 8)))
				(/ 1.0 (lower-bound (make-interval -1 8)))
			)
		)
		(nz-x (make-interval 1 16))
		(nz-y (make-interval 4 19))
	)
	(display "div-interval x y: ")
	(display (div-interval x y))
	(newline)
	(display "div-interval nz-x nz-y: ")
	(display (div-interval nz-x nz-y))
	(newline)
	(display "y-reverse: ")
	(display y-reverse)
	(newline)
)
