#lang racket

(define tolerance 0.00001)
(define (abs x) (cond ((< x 0) (- x)) (else x)))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (iterative-improve close-enough? improve)
	(define (iter guess)
		(let ((next (improve guess)))
			(if (close-enough? guess next)
				next
				(iter next)
			)
		)
	)
	(lambda (guess)
		(iter guess)
	)
)

(define (sqrt x)
	(define (improve guess)
		(average guess (/ x guess)))
	(define (good-enough? guess next)
		(< (abs (- (square guess) x)) 0.001))
	((iterative-improve good-enough? improve) 1.0)
)

; ## point
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (midpoint-segment seg)
	(make-point
		(average (x-point (start-segment seg))
			 (x-point (end-segment seg)))
		(average (y-point (start-segment seg))
			 (y-point (end-segment seg))))
)
; ##

; ## segment
(define (make-segment s e)
	(cons s e)
)

(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (distance seg)
	(let
		(
			(start (start-segment seg))
			(end (end-segment seg))
		)
		; body
		(
			sqrt
			(+ (square (- (x-point end) (x-point start)))
				(square (- (y-point end) (y-point start)))
			)
		)
	)
)

; ##



; ## rect 
; (define (make-rect top right bottom left)
; 	(let
; 		(
; 			(vertical	(cons top bottom))
; 			(horizon	(cons right left))
; 		)
; 		(cons vertical horizon)
; 	)
; )
(define (make-rect top right bottom left)
	(let
		(
			(vertical	(cons top bottom))
			(horizon	(cons right left))
		)
		(cons horizon vertical)
	)
)

(define (top-rect r) (car (car r)))
(define (bottom-rect r) (cdr (car r)))
(define (right-rect r) (car (cdr r)))
(define (left-rect r) (cdr (cdr r)))

(define (perimeter-rect r)
	(+	(distance (top-rect r))
		(distance (bottom-rect r))
		(distance (right-rect r))
		(distance (left-rect r))
	)
)

(define (surface-rect r)
	(*	(distance (top-rect r))
		(distance (right-rect r))
	)
)
; ##

(define seg-top (make-segment
	(make-point 1 3) 
	(make-point 4 3))
)
(define seg-bottom (make-segment
	(make-point 1 1) 
	(make-point 4 1))
)
(define seg-right (make-segment
	(make-point 4 3) 
	(make-point 4 1))
)
(define seg-left (make-segment
	(make-point 1 3) 
	(make-point 1 1))
)

(define rect (make-rect
	seg-top
	seg-right
	seg-bottom
	seg-left
	)
)

; (x-point (car (top-rect rect)))
(perimeter-rect rect)
(surface-rect rect)