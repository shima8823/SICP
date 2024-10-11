#lang racket
(require r5rs) ; set-(car|cdr)!

(define (count-pairs x)
	(define pair-t '())
	(define (again x pair-t)
		(and (not (null? pair-t))
			(or (eq? x (car pair-t))
				(again x (cdr pair-t)))))

	(define (count x)
		; memq = again memq=xがpair-tに含まれていなければfalse
		(if (or (not (pair? x)) (memq x pair-t))
			0
			(begin (set! pair-t (cons x pair-t))
			(+ (count (car x))
			   (count (cdr x))
			1)))
	)
	(count x)
)

(define x4 '(foo)) 
(define y4 (cons x4 x4)) 
(define str4 (list y4)) 
(count-pairs str4) ; 4 

(define x7 '(foo)) 
(define y7 (cons x7 x7)) 
(define str7 (cons y7 y7)) 
(count-pairs str7) ; 7

; (equal? x4)

; test from https://sicp.iijlab.net/solution/ex3.3.html
(define a (cons 'foo 'bar))
(define b (cons a a))
(define l7 (cons b b))
(define l4 (cons a (cons 'foo a)))
(define l3 (cons (cons 'foo 'bar) (cons 'foo 'bar)))
(define c (list 'foo))
(define ll (cons 'foo (cons 'foo c)))
(set-car! c ll)

(newline)(display (count-pairs l7))
(newline)(display (count-pairs l4))
(newline)(display (count-pairs l3))
(newline)(display (count-pairs ll))

