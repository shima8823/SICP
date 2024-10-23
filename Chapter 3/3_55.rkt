#lang sicp
(#%require "./stream_basic_components.rkt")

(define ones (cons-stream 1 ones))
(define integers
	(cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))


(define (partial-sums s)
	(cons-stream
		(stream-car s)
		(add-streams (stream-cdr s) (partial-sums s))))

; 別解 値を保存したままにする。
; (define (partial-sums S) 
; 	(define res (cons-stream (stream-car S) (add-streams (stream-cdr S) res))) 
	; res)

(stream-ref (partial-sums integers) 0)
(stream-ref (partial-sums integers) 1)
(stream-ref (partial-sums integers) 2)
(stream-ref (partial-sums integers) 3)
(stream-ref (partial-sums integers) 4)
(stream-ref (partial-sums integers) 5)
(stream-ref (partial-sums integers) 6)
(stream-ref (partial-sums integers) 7)
(stream-ref (partial-sums integers) 8)
(stream-ref (partial-sums integers) 9)
