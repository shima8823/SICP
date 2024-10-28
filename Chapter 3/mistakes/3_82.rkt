; skip 数学、積分の意味がよくわからなかった。

#lang sicp
(#%require "../stream_basic_components.rkt")
(#%require (only racket/base current-seconds))

(define a 1103515245)
(define b 12345)
(define m (expt 2 31))

(define (square x) (* x x))

(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (random range))))

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

; (define (estimate-integral p x1 x2 y1 y2)
; 	(define (test)
; 		(p x1 x2 y1 y2))
; 	(* (* (- x2 x1) (- y2 y1)) (monte-carlo cesaro-stream 0 0)))
 (define (random-number-pairs low1 high1 low2 high2) 
   (cons-stream (cons (random-in-range low1 high1) (random-in-range low2 high2)) 
                (random-number-pairs low1 high1 low2 high2))) 

(define (estimate-integral p x1 x2 y1 y2) 
	(let ((area (* (- x2 x1) (- y2 y1) 1.0)) 
         (randoms (random-number-pairs x1 x2 y1 y2))) 
     (scale-stream (monte-carlo (stream-map p randoms) 0 0) area))) 

; (define est-integral
; 	(exact->inexact
; 		(estimate-integral
; 			(lambda (x1 x2 y1 y2)
; 				(let ((x (random-in-range x1 x2))
; 					(y (random-in-range y1 y2)))
; 					(<= (+ (square (- x 5)) (square (- y 7))) (square 3))))
; 			2 8 4 10)))

; (define (estimate-pi)
; 	(/ est-integral 9))

;; test. get the value of pi 
 (define (sum-of-square x y) (+ (* x x) (* y y))) 
 (define f 
   (lambda (x) 
     (not (> (sum-of-square (- (car x) 1) (- (cdr x) 1)) 
             1)))) 
 (define pi-stream (estimate-integral f 0 2 0 2)) 
(display-stream-until 100 pi-stream)