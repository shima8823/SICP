#lang sicp
(#%require "./stream_basic_components.rkt")

(define (add-streams s1 s2) (stream-map + s1 s2))

(define fibs
	(cons-stream
		0
		(cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define (stream-display-loop-n n stream)
	(define (iter i)
		(display (stream-ref stream i))(newline)
		(if (= n i)
			'done
			(iter (+ i 1))))
	(iter 0)
)

(stream-display-loop-n 20 fibs)

#|

Fib 2	1
Fib 3	2
Fib 4	4
Fib 5	7
Fib 6	12
Fib 7	20
Fib 8	33
Fib 9	53
Fib 10	86

2^◯ = (
0 = 1
1 = 2
2 = 4
3 = 8
4 = 16
5 = 32
6 = 64
7 = 128
8 = 256
)

ある程度、指数関数的？
※ Fib = O(ϕ^n)

|#