#lang racket

(define x 10)
(parallel-execute
	(lambda () (set! x (* x x)))
	(lambda () (set! x (+ x 1))))
#|
101
121
110
11
100
|#
(define x 10)
(define s (make-serializer))
	(parallel-execute
		(lambda () (set! x ((s (lambda () (* x x))))))
		(s (lambda () (set! x (+ x 1)))))
#|

101
121
11	P1 100をセットする前にp2が始まってp2がセットする前にp1がセット、p2セットでなりそう
100 11と同じ

|#



(define x 10)
	(parallel-execute
		(lambda () (set! x (* x x)))
		(lambda () (set! x (* x x x))))

#|

1000000
1000000

p1 10 1000 p2 10 10 10
10000

p1 10 10 p2 10 10 100
1000
p1 10 10 p2 10 100 100
100000


100
1000

|#

(define x 10)
(define s (make-serializer))
	(parallel-execute
		(s (lambda () (set! x (* x x))))
		(s (lambda () (set! x (* x x x)))))

#|

1000000

|#