#lang racket

(define (cont-frac n d k)
	(define (iter i result)
		(if (= i k)
			result
			(iter (+ i 1) (/ (n i) (+ (d i) result)))
		)
	)
	(iter 1 (/ (n 1) (d 1)))
)

(+
	(cont-frac
			(lambda (i) 1.0)
			(lambda (i) 
				(let ((term (+ (/ (- i 2) 3) 1)))
					(define (sequence?) 
						(integer? term)
					)
					(display term)
					(newline)
					(if (sequence?)
						(* term 2)
						1
					)
				)

			)
			1000
	)
	2
)

#|
i	v
1	1
2	2
5	4
8	6
11	8
14	10
17	12
...
23	


|#