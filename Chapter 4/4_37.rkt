#lang racket

(define (require p) (if (not p) (amb)))
(define (an-element-of items)
	(require (not (null? items)))
	(amb (car items) (an-element-of (cdr items))))

(define (prime-sum-pair list1 list2)
	(let ((a (an-element-of list1))
		  (b (an-element-of list2)))
		(require (prime? (+ a b)))
		(list a b)))

(define (an-integer-starting-from n)
	(amb n (an-integer-starting-from (+ n 1))))


(define (an-integer-between low high)
	(let ((a (an-integer-starting-from low)))
		(require (lambda (x) (<= x high)) a)
		a))

(define (a-pythagorean-triple-between low high)
	(let ((i (an-integer-between low high)))
		(let ((j (an-integer-between i high)))
			(let ((k (an-integer-between j high)))
				(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))

; Ben
(define (a-pythagorean-triple-between low high)
	(let ((i (an-integer-between low high))
		  (hsq (* high high)))
		(let ((j (an-integer-between i high)))
			(let ((ksq (+ (* i i) (* j j))))
				(require (>= hsq ksq))
				(let ((k (sqrt ksq)))
					(require (integer? k))
					(list i j k))))))

#|

Benは正しい。
kをlow ~ highを1つずつ探索していくのと、sqrtで一意に探索するのでは大きく効率が違う。

|#