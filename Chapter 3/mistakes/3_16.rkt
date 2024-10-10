#lang racket
(require rnrs/mutable-pairs-6)

; (define (set-car! pair new-value)
; 	(set! (car pair) new-value))
; (define (set-cdr! pair new-value)
; 	(set! (cdr pair) new-value))

(define (count-pairs x)
	(if (not (pair? x))
		0
		(+ (count-pairs (car x))
		   (count-pairs (cdr x))
		   1)))

 (define q '(a b)) 
 (define r4 (cons q (cdr q))) 
 (count-pairs r4) ; 4 
 r4


(count-pairs '(a b c)) ; 正しい
(count-pairs '((foo) foo foo)) ; 


(count-pairs '(((foo) foo) (foo) foo))
 (define x '(foo)) 
 (define y (cons x x)) 
 (define str2 (list y)) 
str2

(define z4
  (let ([x (list 'a)])
    (list x x)))
	z4

	(define z7
  (let* ([x (list 'a)]
         [y (cons x x)])
    (cons y y)))
	z7
#|


infinity loop = ポインタを輪にする

結論
3つのペアからなるリスト構造がこれらしい
car=value cdr=pointerができるのがペア
(1 2 3)

|#