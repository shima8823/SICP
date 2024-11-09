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
	(require (<= low high))
	(amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low)
	(let ((i (an-integer-starting-from low)))
		(let ((j (an-integer-between low i)))
			(let ((k (an-integer-between low j)))
				(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))

; 効率的にはこっち meteorgan http://community.schemewiki.org/?sicp-ex-4.36
(define (a-pythagorean-triple-greater-than low) 
	(let ((k (an-integer-starting-from low))) 
		(let ((i (an-integer-between low k))) 
			(let ((j (an-integer-between i k))) 
				(require (= (+ (* i i) (* j j)) (* k k))) 
					(list i j k))))) 

#|

単純にan-integer-betweenからan-integer-starting-fromに置き換えるだけだと、
kのambがinfinity try-againされるので適切ではない(i, jがtry-againしない)。

|#