#lang racket

(define (append! x y)
	(set-cdr! (last-pair x) y)
	x)

(define (last-pair x)
	(if (null? (cdr x)) x (last-pair (cdr x))))

(define x (list 'a 'b)) ; '(a b)
(define y (list 'c 'd)) ; '(c d)
(define z (append x y)) z ; '(a b c d)
(cdr x) ; ⟨response⟩ = '(b)
(define w (append! x y)) w ; '(a b c d)
#|

x -> (b1=a b2=(bb1=b bb2=null))
y -> (b1=c b2=(bb1=d bb2=null))
x -> (b1=a b2=(bb1=b bb2=y))

cdr x
x -> b1=b b2=y

|#

(cdr x) ; ⟨response⟩ = '(b c d)