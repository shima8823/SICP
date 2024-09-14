#|

	coin-valuesの順序は関係ない
	なぜなら2つのccが漏れなく実行してるから

	; 補足
	実行時間が降順の場合だと昇順にくらべて遅くなる。

|#

#lang racket

; ## utils list
(define (length items)
	(if (null? items)
		0
		(+ 1 (length (cdr items)))
	)
)

(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2)))
)

(define (reverse l)
	(if (null? l)
		(list)
		(append (reverse (cdr l)) (list (car l)))
	)
)
; ##

(define (except-first-denomination coin-values)
	(cdr coin-values)
)

(define (first-denomination coin-values)
	(car coin-values)
)

(define (no-more? coin-values)
	(= (length coin-values) 0)
)

(define (cc amount coin-values)
	(cond	((= amount 0) 1)
			((or (< amount 0) (no-more? coin-values)) 0)
			(else (+
					(cc 
						amount
						(except-first-denomination coin-values)
					)
					(cc 
						(- amount (first-denomination coin-values))
						coin-values 
					)
				)
			)
	)
)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

(cc 100 (reverse us-coins))
(cc 100 uk-coins)
