; 良問 テイラー級数とは...

#lang sicp
(#%require "../stream_basic_components.rkt")

(define (integrate-series series) (stream-map / series integers))

(define exp-series
	(cons-stream 1 (integrate-series exp-series)))

(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(display-stream-until 6 (integrate-series ones))
(display-stream-until 6 exp-series)
(display-stream-until 6 cosine-series)
(display-stream-until 6 sine-series)

(define (mul-series s1 s2)
	(cons-stream
		(* (stream-car s1) (stream-car s2)) 
		(add-streams
			(scale-stream (stream-cdr s2) (stream-car s1))
			(mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s) 
	(cons-stream
		1
		(scale-stream 
			(mul-series
				(stream-cdr s)
				(invert-unit-series s))
			-1))) 

(newline)
(display-stream-until 6 (invert-unit-series cosine-series))


; (display-stream-until 6 (integrate-series ones))
#|

cos	1 0 -1/2 0 1/24 0 -1/720
S	1 0  1/2 0 5/24 0 61/720

|#