; 良問 leetcodeでやった問題だ...

#lang racket
(require r5rs) ; set-(car|cdr)!

(define (last-pair x)
	(if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x) (set-cdr! (last-pair x) x) x)
(define z (make-cycle (list 'a 'b 'c)))

(define (list-loop? l)
	(define pair-t '())
	(define (loop? l1 l2)
		(and
			(pair? l1)
			(pair? l2)
			(or
				(eq? l1 l2)
				(loop? (cdr l) (cddr l2))
			)
		)
	)
	(and (pair? l)
		 (loop? (cdr l) (cddr l))))

(list-loop? z)

(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1)) 
(list-loop? t2)
