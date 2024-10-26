#lang sicp
(#%require "./stream_basic_components.rkt")

(define (interleave s1 s2)
	(if (stream-null? s1)
		s2
		(cons-stream
			(stream-car s1)
			(interleave s2 (stream-cdr s1)))))

(define (pairs s t)
	(interleave
		(stream-map
			(lambda (x) (list (stream-car s) x))
			t)
		(pairs (stream-cdr s) (stream-cdr t))))

#|

interleaveを実行するのにpairsの答えが必要で
pairsを実行するにはinterleaveが必要だからinifinity loopになる。

|#

(define stream-pair-int (pairs integers integers))
(display-stream-until 0 stream-pair-int)
