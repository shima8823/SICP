#lang racket

(define nil `())

(define (reverse l)
	(display "l : ")
	(display l)
	(newline)

	(if (null? l)
		(list)
		(append (reverse (cdr l)) (list (car l)))
	)
)

(define (deep-reverse l)
	(if (pair? l)
		(append
			(deep-reverse (cdr l))
			(list (deep-reverse (car l)))
		)
		l
	)
)

(define (fringe l)
	(if (pair? l)
		(append
			(if (pair? (car l))
				(fringe (car l))
				(list (fringe (car l)))
			)
			(fringe (cdr l))
		)
		l
	)
)

(define x (list (list 1 2) (list 3 4)))
(define y (list 1 2 3 4))

(fringe x)
(fringe (list x x))
(fringe y)
(fringe '(1 (2 (3 (4 (5 (6 7)))))))

#|

((1 2) (3 4))
(1 2) (3 4) : a b
a: (1 2)
1 (2)
if not pair list(1)
append (1 2)

b: (3 4)
3 (4)
if not pair list(3)
append (3 4)

append (1 2) (3 4)

|#