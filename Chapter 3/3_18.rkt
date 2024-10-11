#lang racket
(require r5rs) ; set-(car|cdr)!

(define (last-pair x)
	(if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x) (set-cdr! (last-pair x) x) x)
(define z (make-cycle (list 'a 'b 'c)))

(define (list-loop? l)
	(define pair-t '())
	(define (loop? l)
		(and (pair? l)
			(or
				(not (not (memq (cdr l) pair-t)))
				(begin (set! pair-t (cons l pair-t))
					(loop? (cdr l)))
			)
		)
	)
	(loop? l)
)

(list-loop? z)

(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1)) 
(list-loop? t2)
