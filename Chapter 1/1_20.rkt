#|

正規順序評価
(gcd 206 40)
(gcd 40 (remainder 206 40)) b = 6
(gcd (remainder 206 40) (remainder 40 (remainder 206 40))) b = 4
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) b = 2
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) b = 0

remainder 18回

適正順序評価
(gcd 206 40)
(gcd 40 6) b = 6
(gcd 6 4) b = 4
(gcd 4 2) b = 2
(gcd 2 0) b = 0
a. 2

remainder 4回

|#

#lang racket

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))

(gcd 206 40)
