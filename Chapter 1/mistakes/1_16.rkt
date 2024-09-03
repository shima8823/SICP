#|

一般的に、 各状態を通して一定であるような
不変量(invariant quantity)を定義するというテクニックは、
反復アルゴリズムを設計するうえで 強力な方法である。)

|#

#lang racket

(define (fast-expt b n) (fast-expt-iter 1 b n))
(define (fast-expt-iter a b n)
	(cond	((= n 0) a)
			((even? n) (fast-expt-iter a (* b b) (/ n 2)))
			(else (fast-expt-iter (* a b) b (- n 1)))))

(define (even? n)
	(= (remainder n 2) 0))

(fast-expt 2 10)