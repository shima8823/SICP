#lang sicp
(#%require "./stream_basic_components.rkt")

(define (scale-stream stream factor)
	(stream-map
		(lambda (x) (* x factor))
		stream))

(define (merge s1 s2)
	(cond ((stream-null? s1) s2)
		  ((stream-null? s2) s1)
		  (else
			(let ((s1car (stream-car s1))
				  (s2car (stream-car s2)))
				(cond
					((< s1car s2car)
						(cons-stream
							s1car
							(merge (stream-cdr s1) s2)))
					((> s1car s2car)
						(cons-stream
							s2car
							(merge s1 (stream-cdr s2))))
					(else
						(cons-stream
							s1car
							(merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 5) (merge (scale-stream S 2) (scale-stream S 3)))))

(define (stream-display-loop-n n stream)
	(define (iter i)
		(display (stream-ref stream i))(newline)
		(if (= n i)
			'done
			(iter (+ i 1))))
	(iter 0)
)

(stream-display-loop-n 40 S)