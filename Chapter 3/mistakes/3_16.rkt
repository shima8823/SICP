#lang racket
(require r5rs)

(define (count-pairs x)
	(if (not (pair? x))
		0
		(+ (count-pairs (car x))
		   (count-pairs (cdr x))
		   1)))

(define x4 '(foo)) 
(define y4 (cons x4 x4)) 
(define str4 (list y4)) 
(count-pairs str4) ; 4 

(define x7 '(foo)) 
(define y7 (cons x7 x7)) 
(define str7 (cons y7 y7)) 
(count-pairs str7) ; 7

(define x-inf '(foo))
(define y-inf (cons x-inf x-inf))
(define str-inf (list y-inf))
(set-car! x-inf str-inf)
(count-pairs str-inf)

#|

infinity loop = ポインタを輪にする

結論
3つのペアからなるリスト構造がこれらしい
car=value cdr=pointerができるのがペア
(1 2 3)

|#