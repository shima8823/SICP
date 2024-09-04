#lang racket

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
		(cond	((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (next test-divisor)))
		)
)

(define (next test-divisor)
	(if (= test-divisor 2)
		3
		(+ test-divisor 2)
	)
)

(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (timed-prime-test n)
		(newline)
		(display n)
		(start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
		(cond ((prime? n)
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


(define (search-for-primes lower-bound upper-bound)
	(cond	((<= lower-bound upper-bound)
		(cond ((prime? lower-bound)
				(display lower-bound)
				(newline)
			)
		)
		(search-for-primes (+ 1 lower-bound) upper-bound)
	))
)

; (search-for-primes 1000 10000)
; 3109
; 7027
; 9091
; (search-for-primes 10000 100000)
; 19997
; 73999
; 99929
; (search-for-primes 100000 1000000)
; 192391
; 579949
; 995381
; (search-for-primes 1000000 10000000)
; 1999979
; 5999933
; 9999971

(timed-prime-test 3109)		; a1
(timed-prime-test 7027)		; a2
(timed-prime-test 9091)		; a3

(timed-prime-test 19997)	; b1
(timed-prime-test 73999)	; b2
(timed-prime-test 99929)	; b3

(timed-prime-test 192391)	; c1
(timed-prime-test 579949)	; c2
(timed-prime-test 995381)	; c3

(timed-prime-test 1999979)	; d1
(timed-prime-test 5999933)	; d2
(timed-prime-test 9999971)	; d3

#|

prime		1_22
			1_23
3109	*** 0.003173828125
			0.001953125

7027	*** 0.001953125	
			0.0009765625

9091	*** 0.0029296875
			0.0009765625

19997	*** 0.003173828125
			0.002197265625

73999	*** 0.02490234375
			0.004150390625

99929	*** 0.009033203125
			0.0029296875

192391	*** 0.010009765625
			0.006103515625

579949	*** 0.027099609375
			0.010986328125

995381	*** 0.025146484375
			0.010986328125

1999979	*** 0.06201171875
			0.014892578125

5999933	*** 0.06787109375
			0.034912109375

9999971	*** 0.099853515625
			0.06396484375%

概ね2倍である

|#
