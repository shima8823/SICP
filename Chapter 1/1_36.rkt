#lang racket

(define (average x y)
	(/ (+ x y) 2.0)
)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2) 
		(< (abs (- v1 v2))
			tolerance))
	(define (try guess)
		(let ((next (f guess)))
		(display next)
		(newline)
		(if (close-enough? guess next)
			next
			(try next)))
	)
	(try first-guess)
)

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
; 35steps
(newline)
; 平均緩和法(average damping)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)
; 10steps