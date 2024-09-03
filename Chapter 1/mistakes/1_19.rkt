#|

Tp'q' = Tpq(Tpq(a,b))(Tpqを2回適用する)
a = bq+aq+ap, b = bp+aq
p′= p^2+q^2
q′= 2qp+q^2

|#

#lang racket

(define (fib n)
	(fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
	(cond	((= count 0) b)
			((even? count) (fib-iter a
									b
									(+ (* p p) (* q q))  ; p'を計算
									(+ (* 2 p q) (* q q)); q'を計算
									(/ count 2)))
			(else (fib-iter (+ (* b q) (* a q) (* a p))
							(+ (* b p) (* a q))
							p
							q
							(- count 1)))))

(define (even? n)
	(= (remainder n 2) 0))

(fib 10)