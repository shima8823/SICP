; もうちょっと詳細に問題をかいてほしい

#lang racket

(define (average x y)
	(/ (+ x y) 2.0)
)

(define (make-segment s e)
	(cons s e)
)

(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (midpoint-segment seg)
	(make-point
		(average (x-point (start-segment seg))
			 (x-point (end-segment seg)))
		(average (y-point (start-segment seg))
			 (y-point (end-segment seg))))
)

(define (print-point p)
	(newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")")
)

(define seg (make-segment	(make-point 2 3) 
							(make-point 10 15))
)
  
(print-point (midpoint-segment seg)) 
