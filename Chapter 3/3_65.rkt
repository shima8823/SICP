#lang sicp
(#%require "./stream_basic_components.rkt")

(define (average x y) (/ (+ x y) 2.0))
(define (square x) (* x x))

(define (partial-sums S) 
	(define res
		(cons-stream
			(stream-car S)
			(add-streams (stream-cdr S) res))) 
	res)

(define (euler-transform s)
	(let ((s0 (stream-ref s 0))  ; Sn−1
		  (s1 (stream-ref s 1))  ; Sn
		  (s2 (stream-ref s 2))) ; Sn+1
		(cons-stream
			(-
				s2
				(/ (square (- s2 s1))
				   (+ s0 (* -2 s1) s2)))
	(euler-transform (stream-cdr s)))))

(define (ln2-summands n)
	(cons-stream
		(/ 1.0 n)
		(stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
	(partial-sums (ln2-summands 1)))

(define (make-tableau transform s)
	(cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
	(stream-map stream-car (make-tableau transform s)))

(display-stream-until 100 ln2-stream)
(display-stream-until 10 (euler-transform ln2-stream))
(display-stream-until 10 (accelerated-sequence euler-transform ln2-stream))


#|

acceleratedの3項目ですでにeuler-transformの10項目の値が出現している。
eulerの2項目ですでに素のln2-streamの100項目値の値が出現している。

|#
