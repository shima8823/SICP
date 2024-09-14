#lang racket

(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (= (remainder x 2) 1))
(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2)))
)

(define (same-parity x . l)
	(define (make-list valid? l) ; return list
		(if (null? l)
			(list)
			(if (valid? (car l))
				(append (list (car l)) (make-list valid? (cdr l)))
				(make-list valid? (cdr l))
			)
			; (if (valid? (car l))
			; 	(cons (car l) (make-list valid? (cdr l)))
			; 	(make-list valid? (cdr l))
			; )
		)
	)

	(if (even? x)
		(cons x (make-list even? l))
		(cons x (make-list odd? l))
	)
)

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)