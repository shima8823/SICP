#lang sicp
(#%require "./stream_basic_components.rkt")

(define (interleave s1 s2)
	(if (stream-null? s1)
		s2
		(cons-stream
			(stream-car s1)
			(interleave s2 (stream-cdr s1)))))

#|

(interleave ((S0,T1) (S0,T2) ...) ((S1,T1) (S1,T2)...\n (S2,T2)...))
call
(S0,T1)
(interleave ((S1,T1) (S1,T2)...\n (S2,T2)...) ((S0,T2) ...))
(S1,T1)
(interleave ((S0,T2) ...) ((S1,T2)...\n (S2,T2)...))
(S0,T2)
(interleave ((S1,T2)...\n (S2,T2)...) (...))
(S1,T2)
	(interleave ((S2,T2)... ...\n) (...))
	(S2,T2)
(interleave (...\n) (...))

|#

(define (pairs s t)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(interleave
			(stream-map
				(lambda (x) (list (stream-car s) x))
				(stream-cdr t))
			(pairs (stream-cdr s) (stream-cdr t)))))