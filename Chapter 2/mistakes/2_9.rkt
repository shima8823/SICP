#|

; width = 5
x = 3 ~ 11
(width x) = 6 ~ 11
y = 1 ~ 9
(width y) = 2 ~ 7

(add-width x y) = 8 ~ 18
(sub-width x y) = 1 ~ 9
(mul-width x y) = 12 ~ 77
(div-width x y) = 6/7 ~ 11/2
(width x) = 6 ~ 11
y = 1/7 ~ 1 / 2
p1 6/7
p2 3
p3 11/7
p4 11/2

|#

#lang racket

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (max (car interval) (cdr interval)))
(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (add-interval x y)
	(make-interval
		(+ (lower-bound x) (lower-bound y))
		(+ (upper-bound x) (upper-bound y))
	)
)

(define (sub-interval x y)
	(make-interval
		(- (lower-bound x) (upper-bound y))
		(- (upper-bound x) (lower-bound y))
	)
)

(define (mul-interval x y)
	(let (
		(p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y)))
		)
		(make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
	)
)

(define (div-interval x y)
	(mul-interval
		x
		(make-interval
			(/ 1.0 (upper-bound y))
			(/ 1.0 (lower-bound y))
		)
	)
)

(define (width i)
	(/ (- (upper-bound i) (lower-bound i)) 2)
)

(let
	(
		(x (make-interval 3 11))
		(y (make-interval 1 9))
	)
	(display "width x: ")
	(display (width x))
	(newline)
	(display "width y: ")
	(display (width y))
	(newline)
	(display "width x + y: ")
	(display (+ (width x) (width y)))
	(newline)

	(display "width (add-interval x y): ")
	(display (width (add-interval x y)))
	(newline)

	(display "width (sub-interval x y): ")
	(display (width (sub-interval x y)))
	(newline)

	(display "width (mul-interval x y): ")
	(display (width (mul-interval x y)))
	(newline)

	(display "width (div-interval x y): ")
	(display (width (div-interval x y)))
	(newline)
)

#|

関数とは((width x) + (width y))のこと

https://sicp.iijlab.net/solution/ex2.1.html より

区間 a の幅 awは(a↑-a↓)/2.　区間 x と区間 y の和 x+y の幅は
(x+y)w=((x+y)↑-(x+y)↓)/2=((x↑+y↑)-(x↓+y↓))/2
=(x↑-x↓)/2 + (y↑-y↓)/2=xw+yw
区間 x と区間 y の差 x-y の幅は
(x-y)w=((x-y)↑-(x-y)↓)/2=((x↑-y↓)-(x↓-y↑))/2
=(x↑-x↓)/2 + (y↑-y↓)/2=xw+yw
乗算, 除算についてはテストしてみると
(define a (make-interval 9 11))
(width a) ==> 1
(define b (make-interval 1 3))
(width b) ==> 1
(define c (make-interval 19 21))
(width c) ==> 1
(width (mul-interval a b)) ==> 12
(width (mul-interval a c)) ==> 30
(width (div-interval a b)) ==> 4

|#
