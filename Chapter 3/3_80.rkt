#lang sicp
(#%require "./stream_basic_components.rkt")

(define (integral delayed-integrand initial-value dt)
	(define int
		(cons-stream
			initial-value
			(let ((integrand (force delayed-integrand)))
				(add-streams (scale-stream integrand dt) int))))
	int)

(define (RLC R L C dt)
	(lambda (il0 vc0)
		(define vc (integral (delay (dvc)) vc0 dt))
		(define il (integral (delay (dil)) il0 dt))
		(define (dvc) (scale-stream il (/ -1 C)))
		(define (dil)
			(add-streams
				(scale-stream vc (/ 1 L))
				(scale-stream il (/ (* R -1) L))))
		; 返す値は"ストリームのペア"なので
		; (cons vc il)
		; だが、出力を見るにはmapを使った方が良い。
		(stream-map cons vc il)))

(define RLC1 (RLC 1 1 0.2 0.1))
(display-stream-until 10 (RLC1 0 10))