#lang sicp
(#%require "./stream_basic_components.rkt")

(define (square x) (* x x))

(define (interleave s1 s2)
	(if (stream-null? s1)
		s2
		(cons-stream
			(stream-car s1)
			(interleave s2 (stream-cdr s1)))))

(define (weighted-pairs s t weight)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(merge-weighted
			(stream-map
				(lambda (x) (list (stream-car s) x))
				(stream-cdr t))
			(weighted-pairs (stream-cdr s) (stream-cdr t) weight)
			weight)))

(define (merge-weighted s1 s2 weight)
	; (display s1)(display " s2 ")(display s2)(newline)
	(cond ((stream-null? s1) s2)
		  ((stream-null? s2) s1)
		  (else
			(let ((s1car (stream-car s1))
				  (s2car (stream-car s2))
				  (s1car-w (weight (stream-car s1)))
				  (s2car-w (weight (stream-car s2))))
				(cond
					((< s1car-w s2car-w)
						; (display s1car-w)(display " s2 ")(display s2car)(newline)
						(cons-stream
							s1car
							(merge-weighted (stream-cdr s1) s2 weight)))
					(else
						(cons-stream
							s2car
							(merge-weighted s1 (stream-cdr s2) weight))))))))

(define a (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))
(display-stream-until 20 a)

(define b
	(weighted-pairs
		(stream-filter
			(lambda (x)
				(not (or
					(= (remainder x 2) 0)
					(= (remainder x 3) 0)
					(= (remainder x 5) 0))))
			integers)
		(stream-filter
			(lambda (x)
				(not (or
					(= (remainder x 2) 0)
					(= (remainder x 3) 0)
					(= (remainder x 5) 0))))
			integers)
		(lambda (x)
			(let ((i (car x))
				  (j (cadr x)))
				(+ (* 2 i) (* 3 j) (* 5 i j))))))
(display-stream-until 20 b)
