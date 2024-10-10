#lang racket

(define (mystery x)
	(define (loop x y)
		(if (null? x)
			y
			(let ((temp (cdr x)))
				(set-cdr! x y)
				(loop temp x))))
	(loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

#|

v -> (a (b (c (d null)))) = '(a b c d)
補足
v -> (a)の可能性もあるがinterpreterの実装に依存する

(loop (a b c d) ())
temp='(b c d)
x='(a null)
(loop (b c d) x)
temp='(c d)
x='(b (a null))
(loop (c d) x)
temp='(d)
x='(c (b (a null)))
...
w='(d (c (b (a null))))

w -> (d (c (b (a null))))



|#