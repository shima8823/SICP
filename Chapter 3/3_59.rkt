; 良問 実際のモデル、無限級数

#lang sicp
(#%require "./stream_basic_components.rkt")

(define (integrate-series series)
	(cons-stream
		(stream-car series)
		(mul-streams (div-streams ones (stream-cdr integers)) (stream-cdr series))
	))

(define exp-series
	(cons-stream 1 (integrate-series exp-series)))

(define minuss (cons-stream -1 minuss))

(define cosine-series (cons-stream 1 (integrate-series (mul-streams minuss sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(display-stream-until 6 (integrate-series ones))
(display-stream-until 6 exp-series)
(display-stream-until 6 cosine-series)
(display-stream-until 6 sine-series)

#|

別解
 (define (integrate-series s) 
   (stream-map /  s integers)) 

 (define cosine-series 
   (cons-stream 1 (stream-map - (integrate-series sine-series)))) 
	 			or (integrate-series (scale-stream sine-series -1))
|#