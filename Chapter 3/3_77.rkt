#lang sicp
(#%require "./stream_basic_components.rkt")

; (define (integral delayed-integrand initial-value dt)
; 	(define int
; 		(cons-stream
; 			initial-value
; 			(let ((integrand (force delayed-integrand)))
; 				(add-streams (scale-stream integrand dt) int))))
; 	int)

(define (integral delayed-integrand initial-value dt)
	(cons-stream
		initial-value
		(let ((integrand (force delayed-integrand)))
			; (display delayed-integrand)
			(if (stream-null? integrand)
				the-empty-stream
				(integral
					(delay (stream-cdr integrand))
					(+ (* dt (stream-car integrand))
						initial-value)
					dt)))))

(define (solve f y0 dt)
  (define y (integral (delay (dy)) y0 dt))
  (define (dy) (stream-map f y))
  y)

(stream-ref
	(solve
		(lambda (y) y)
		1
		0.001)
	1000)
