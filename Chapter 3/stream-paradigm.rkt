#lang sicp
(#%require "./stream_basic_components.rkt")

(define (average x y) (/ (+ x y) 2.0))
(define (square x) (* x x))

(define (sqrt-improve guess x)
	(average guess (/ x guess)))

(define (sqrt-stream x)
	(define guesses
		(cons-stream
			1.0
			(stream-map
				(lambda (guess) (sqrt-improve guess x))
				guesses)))
	guesses)
(display-stream-until 3 (sqrt-stream 2))

(define (partial-sums S) 
	(define res
		(cons-stream
			(stream-car S)
			(add-streams (stream-cdr S) res))) 
	res)

(define (pi-summands n)
	(cons-stream
		(/ 1.0 n)
		(stream-map - (pi-summands (+ n 2)))))

(define pi-stream
	(scale-stream (partial-sums (pi-summands 1)) 4))
; (display-stream pi-stream)
(display-stream-until 10 pi-stream)

(define (euler-transform s)
	(let ((s0 (stream-ref s 0))  ; Sn−1
		  (s1 (stream-ref s 1))  ; Sn
		  (s2 (stream-ref s 2))) ; Sn+1
		(cons-stream
			(-
				s2
				(/ (square (- s2 s1))
				   (+ s0 (* -2 s1) s2)))
	(euler-transform (stream-cdr s)))))

(display-stream-until 10 (euler-transform pi-stream))

(define (make-tableau transform s)
	(cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
	(stream-map stream-car (make-tableau transform s)))

(display-stream-until 10 (accelerated-sequence euler-transform pi-stream))