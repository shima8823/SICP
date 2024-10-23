#lang sicp
(#%require "./stream_basic_components.rkt")

(define ones (cons-stream 1 ones))
(define integers
	(cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))


(define factorials
	(cons-stream
		1
		(mul-streams factorials (add-streams integers ones))))

(stream-ref factorials 0)
(stream-ref factorials 1)
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 4)
(stream-ref factorials 5)
(stream-ref factorials 6)
(stream-ref factorials 7)
(stream-ref factorials 8)
(stream-ref factorials 9)

