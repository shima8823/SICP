#lang racket

(define nil `())

(define (abs x)
	(cond ((< x 0) (- x))
		(else x)))

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence))
		)
	)
)

(define (fold-right op initial sequence)
	(accumulate op initial sequence)
)

(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest)) (cdr rest))
		)
	)
	(iter initial sequence)
)

#|

(fold-right / 1 (list 1 2 3))
1 / (2 / (3 / 1))
a. 3 / 2

(fold-left / 1 (list 1 2 3))
((1 / 1) / 2) / 3
a. 1 / 6

(fold-right list nil (list 1 2 3))
(list 1 (list 2 (list 3 ())))
a. (1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
(list (list (list () 1) 2) 3)
a.(((() 1) 2) 3)

Q.fold-rightとfold-leftが任意の列に対して
同じ値を返すことを保証するために、
opが満たさなければならない性質

A.
組み合わせる順序が結果に関係しない演算
+, *, etc
補足
=二項演算が可換 a op b = b op a

|#

(fold-right / 1 (list 1 2 3)) (fold-left / 1 (list 1 2 3)) (fold-right list nil (list 1 2 3)) (fold-left list nil (list 1 2 3))

(newline)
(fold-right + 0 (list 1 2 3 4))
(fold-left + 0 (list 1 2 3 4))
(fold-right * 1 (list 1 2 3 4))
(fold-left * 1 (list 1 2 3 4))
