#lang racket

(define nil `())
(define (square x) (* x x))

#|

Q. なぜ逆になるか
A. listの構築が逆になっているから

1
(iter cdr (1 `()))
2
(iter cdr (4 (1 `())))
3
(iter cdr (9 (4 (1 `()))))
4
(iter cdr (16 (9 (4 (1 `())))))
nil
(iter cdr (16 (9 (4 (1 `())))))

|#
(define (square-list items)
	(define (iter things answer)
		(if (null? things) 
			answer
			(iter (cdr things)
				(cons 
					(square (car things))
					answer
				)
			)
		)
	)
	(iter items nil)
)

(square-list (list 1 2 3 4))

#|

Q. なぜ'((((() . 1) . 4) . 9) . 16)になるか
A. list: 値 = pairになるから

; 値 : list = list
; list: list = list(list)
; list: 値 = pair
|#
(define (fix-square-list items)
	(define (iter things answer)
		(if (null? things)
			answer
			(iter
				(cdr things)
				(cons answer
					(square (car things))
				)
			)
		)
	)
	(iter items nil)
)

(fix-square-list (list 1 2 3 4))
