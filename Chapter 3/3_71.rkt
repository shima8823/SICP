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

(define (add-cubic x)
	(let ((i (car x))
		(j (cadr x)))
		(+ (* i i i) (* j j j))))

(define add-cubic-order-stream
	(weighted-pairs
		integers
		integers
		(lambda (x) (add-cubic x))))

; (display-stream-until 20 add-cubic-order-stream)


(define (stream-ramanujan-filter stream)
	(cond
		((stream-null? stream) the-empty-stream)
		((= (add-cubic (stream-car stream)) (add-cubic (stream-car (stream-cdr stream))))
			(display (stream-car stream))(display " ")(display (stream-car (stream-cdr stream)))
			(cons-stream
				(add-cubic (stream-car stream))
				(stream-ramanujan-filter
					(stream-cdr stream))))
		(else (stream-ramanujan-filter (stream-cdr stream)))))
(define ramanujan
	(stream-ramanujan-filter add-cubic-order-stream))

(display-stream-until 20 ramanujan)

