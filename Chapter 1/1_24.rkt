#lang racket

(define (expmod base exp m)
	(cond ((= exp 0) 1)
	((even? exp)
		(remainder
			(square (expmod base (/ exp 2) m))
			m))
	(else
		(remainder
			(* base (expmod base (- exp 1) m))
			m))
	)
)

(define (square x) (* x x))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a)
	)
	(try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
	(cond	((= times 0) true)
			((fermat-test n) (fast-prime? n (- times 1)))
			(else false)
	)
)

(define (timed-prime-test n)
		(newline)
		(display n)
		(start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
		(cond ((fast-prime? n 10000)
			(report-prime (- (runtime) start-time)))
		)
)

(define (report-prime elapsed-time)
		(display " *** ")
		(display elapsed-time)
)

;https://stackoverflow.com/questions/2195105/is-there-an-equivalent-to-lisps-runtime-primitive-in-scheme
(define (runtime)
	(current-inexact-milliseconds)
)

(timed-prime-test 3109)		; a1
(timed-prime-test 7027)		; a2
(timed-prime-test 9091)		; a3

(timed-prime-test 1999979)	; d1
(timed-prime-test 5999933)	; d2
(timed-prime-test 9999971)	; d3

#|

予想
1000(log2 1000)				= 9.965784284662087
1,000,000(log2 1,000,000)	= 19.931568569324174

10 / 20 = 1/2
1,000,000は1000に比べて2倍かかる

結果
3109	*** 15.739990234375
7027	*** 22.10205078125
9091	*** 137.69580078125
1999979 *** 46.5400390625
5999933 *** 107.426025390625
9999971 *** 44.075927734375

ずれがある。
フェルマーテストのrandomでテストする時に、片方に大きい値が出て、もう片方に小さな値が出る可能性があり、手続きexpmodで実行スピードが変わるため。
フェルマーテストの試行回数を減らしていくとおおよそ2倍遅いことがわかる。

|#