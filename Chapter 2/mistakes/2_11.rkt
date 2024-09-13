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

#|
こういう時は実際にあり得る組み合わせを数値で入れるのではなく記号を用いた方がわかりやすい

x = 3 ~ 10
y = 1 ~ 5

x = 3 ~ 10
y = -1 ~ 5

x = -3 ~ 10
y = 1 ~ 5


min = p1 =	lx > 0 & ly > 0
			lx > 0 & ly < 0
			lx < 0 & ly > 0

x = -3 ~ -2
y = -5 ~ 1

x = -3 ~ -2
y = 1 ~ 5

min = p2 =	ux < 0 & ly < 0 & uy > 0
			lx < 0 & uy > 0
min = p3 =	lx < 0 & uy < 0

x = -10 ~ -3
y = -5 ~ -1

min = p4 =	ux < 0 & uy < 0


minus	x1 = lx < 0 & ux < 0
zero	x2 = lx < 0 & 0 < ux
plus	x3 = 0 < lx & 0 < ux

minus	y1 = ly < 0 & uy < 0
zero	y2 = ly < 0 & 0 < uy
plus	y3 = 0 < ly & 0 < uy

	y1	y2	y3
x1	
x2
x3

|#

(define (new-mul-interval x y)
	(let (
		(lx (lower-bound x))
		(ux (upper-bound x))
		(ly (lower-bound y))
		(uy (upper-bound y))
		)
		(cond 
			((and (< lx 0) (< ux 0))
				(cond
					((and (< ly 0) (< uy 0))
						(make-interval (* ux uy) (* lx ly))
					)
					((and (< ly 0) (< 0 uy))
						(make-interval (* lx uy) (* lx ly))
					)
					((and (< 0 ly) (< 0 uy))
						(make-interval (* lx uy) (* ux ly))
					)
				)
			)
			(
				(and (< lx 0) (< 0 ux))
				(cond
					((and (< ly 0) (< uy 0))
						(make-interval (* ux ly) (* lx ly))
					)
					((and (< ly 0) (< 0 uy))
						(make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy)))
					)
					((and (< 0 ly) (< 0 uy))
						(make-interval (* lx uy) (* ux uy))
					)

				)
			)
			(
				(and (< 0 lx) (< 0 ux))
				(cond
					((and (< ly 0) (< uy 0))
						(make-interval (* ux ly) (* lx uy))
					)
					((and (< ly 0) (< 0 uy))
						(make-interval (* ux ly) (* ux uy))
					)
					((and (< 0 ly) (< 0 uy))
						(make-interval (* lx ly) (* ux uy))
					)

				)
			)
		)
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

; (let
; 	(
; 		(x (make-interval -1 14))
; 		(y (make-interval -1 8))
; 		(nz-x (make-interval 1 16))
; 		(nz-y (make-interval 4 19))
; 	)
; 	(display "mul-interval x y: ")
; 	(display (mul-interval x y))
; 	(newline)
; 	(display "mul-interval nz-x nz-y: ")
; 	(display (mul-interval nz-x nz-y))
; 	(newline)
; 	(newline)
; 	(display "new-mul-interval x y: ")
; 	(display (new-mul-interval x y))
; 	(newline)
; 	(display "new-mul-interval nz-x nz-y: ")
; 	(display (new-mul-interval nz-x nz-y))
; 	(newline)
; )

(define xp (make-interval 2 3))
(define yp (make-interval 4 5))
(define xm (make-interval -5 -4))
(define ym (make-interval -3 -2))
(define xz (make-interval -2 1))
(define yz (make-interval -1 2))

(display (new-mul-interval xp yp)) (display (mul-interval xp yp)) (newline)
(display (new-mul-interval xp yz)) (display (mul-interval xp yz)) (newline)
(display (new-mul-interval xp ym)) (display (mul-interval xp ym)) (newline)
(display (new-mul-interval xz yp)) (display (mul-interval xz yp)) (newline)
(display (new-mul-interval xz yz)) (display (mul-interval xz yz)) (newline)
(display (new-mul-interval xz ym)) (display (mul-interval xz ym)) (newline)
(display (new-mul-interval xm yp)) (display (mul-interval xm yp)) (newline)
(display (new-mul-interval xm yz)) (display (mul-interval xm yz)) (newline)
(display (new-mul-interval xm ym)) (display (mul-interval xm ym)) (newline)