#lang sicp
(#%require "./stream_basic_components.rkt")
(#%require (only racket/base current-seconds))

(define a 1103515245)
(define b 12345)
(define m (expt 2 31))


(define random-init (remainder (current-seconds) m))
(define (rand-update x)
	(remainder (+ (* a x) b) m))

(define random-numbers
	(cons-stream
		random-init
		(stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
	(cons-stream
		(f (stream-car s) (stream-car (stream-cdr s)))
		(map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
	(map-successive-pairs
		(lambda (r1 r2) (= (gcd r1 r2) 1))
		random-numbers))

(define (monte-carlo experiment-stream passed failed)
	(define (next passed failed)
		(cons-stream
			(/ passed (+ passed failed))
			(monte-carlo 
				(stream-cdr experiment-stream)
				passed
				failed)))
		(if (stream-car experiment-stream)
			(next (+ passed 1) failed)
			(next passed (+ failed 1))))

(define pi
	(stream-map
		(lambda (p) (sqrt (/ 6 p)))
		(monte-carlo cesaro-stream 0 0)))

; (display-stream-until 100 cesaro-stream)
(stream-ref pi 1000)