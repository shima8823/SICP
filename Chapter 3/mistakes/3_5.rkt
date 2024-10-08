; 良問

#lang racket

(define (square x) (* x x))

(define (monte-carlo trials experiment)
	(define (iter trials-remaining trials-passed)
		(cond ((= trials-remaining 0) 
				(/ trials-passed trials))
			  ((experiment)
				(iter (- trials-remaining 1)
					(+ trials-passed 1)))
			  (else
				(iter (- trials-remaining 1) trials-passed ))))
	(iter trials 0))

(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
	(define (test)
		(p x1 x2 y1 y2))
	(* (* (- x2 x1) (- y2 y1)) (monte-carlo trials test)))

(define est-integral
	(exact->inexact
		(estimate-integral
			(lambda (x1 x2 y1 y2)
				(let ((x (random-in-range x1 x2))
					(y (random-in-range y1 y2)))
					(<= (+ (square (- x 5)) (square (- y 7))) (square 3))))
			2 8 4 10 10000)))

(define (estimate-pi)
	(/ est-integral 9))
est-integral
(estimate-pi)