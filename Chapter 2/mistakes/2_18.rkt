#lang racket

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

(reverse (list 1 4 9 16 25))

(define one-through-four (list 1 2 3 4))
(cons (car one-through-four) one-through-four)	; 値 : list = list
(cons one-through-four one-through-four)		; list: list = list(list)
(cons one-through-four (car one-through-four))	; list: 値 = pair

#|

25 : (1 4 9 16)

|#