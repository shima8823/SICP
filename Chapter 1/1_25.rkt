#|



|#
#lang racket

(define (expmod base exp m)
	(remainder (fast-expt base exp) m)
)

(define (fast-expt b n)
	(cond	((= n 0) 1)
			((even? n) (square (fast-expt b (/ n 2))))
			(else (* b (fast-expt b (- n 1))))
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

1_24のプログラムと1_25のプログラムの実行速度を比べると
こちらのほうがとても遅く、素数判定に使えない。
理由は1_24の方は指数を直接計算せずに指数を2で割っているのに対し
1_25の方はとても大きい指数の数を計算してから剰余を求めているから。


|#
