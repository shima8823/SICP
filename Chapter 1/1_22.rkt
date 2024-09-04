#lang racket

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
		(cond	((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (+ test-divisor 1)))
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
; (timed-prime-test 19999)

; (search-for-primes 1000 10000)
; 3109
; 7027
; 9091
; (search-for-primes 10000 100000)
; 19997
; 73999
; 99929
; (search-for-primes 100000 100000)
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
√10 = 3.16227766017
a1 * √10 = b1
√10 = b1 / a1

b1 / a1 = 1.0833333333333333
b2 / a2 = 2.625
b3 / a3 = 4.0

c1 / b1 = 3.1538461538461537
c2 / b2 = 3.142857142857143
c3 / b3 = 2.6875

素数の値が小さいと√10倍に満たさないときがあるが、
ステップ数に比例して実行時間が増えるという概念は
概ね矛盾していない。

|#