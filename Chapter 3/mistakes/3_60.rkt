; 良問 数学の表現が面白い

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

; (define (mul-series s1 s2)
; 	(cons-stream
; 		(* (stream-car s1) (stream-car s2))
; 		(add-streams
; 			(add-streams
; 				(mul-streams s1 (stream-cdr s2))
; 				(mul-streams s2 (stream-cdr s1)))
; 			(mul-streams (stream-cdr s1) (stream-cdr s2)))))

; answer
 (define (mul-series s1 s2) 
	(cons-stream (* (stream-car s1) (stream-car s2)) 
					(add-streams 
					(scale-stream (stream-cdr s2) (stream-car s1)) 
					(mul-series (stream-cdr s1) s2)))) 

(define (sine-cosine-addtion-theorem x)
	(add-streams
		(mul-streams
			(mul-series
				sine-series
				sine-series)
			x)
		(mul-streams
			(mul-series
				cosine-series
				cosine-series)
			x)
		))

(display-stream-until 6 (sine-cosine-addtion-theorem ones))
