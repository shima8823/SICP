#lang sicp
(#%require "./stream_basic_components.rkt")

; (define (solve f y0 dt)
; 	(define y (integral dy y0 dt))
; 	(define dy (stream-map f y))
; 	y)

(define (integral delayed-integrand initial-value dt)
	(define int
		(cons-stream
			initial-value
			(let ((integrand (force delayed-integrand)))
				(add-streams (scale-stream integrand dt) int))))
	int)

; (define (solve f y0 dt)
; 	(define y (integral (delay dy) y0 dt))
; 	(define dy (stream-map f y))
; 	y)

; https://github.com/sicp-lang/sicp/issues/28
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
