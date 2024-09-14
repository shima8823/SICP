; 抵抗の計算に興味が出ない

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

(define (div-interval x y)
	(mul-interval
		x
		(make-interval
			(/ 1.0 (upper-bound y))
			(/ 1.0 (lower-bound y))
		)
	)
)

(define (make-center-width c w)
	(make-interval (- c w) (+ c w)))
(define (center i)
	(/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
	(/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p) ;center percent
	(make-center-width c (/ (* c (/ p 100)) 2)))
(define (percent i)
	(* (/ (- (upper-bound i) (lower-bound i)) (center i)) 100))


(define (par1 r1 r2)
	(div-interval 
		(mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2)
	(let ((one (make-interval 1 1)))
		(div-interval
			one
			(add-interval
				(div-interval one r1)
				(div-interval one r2)))))

(define xp (make-interval 2 3))
(define yp (make-interval 4 5))
(define xm (make-interval -5 -4))
(define ym (make-interval -3 -2))
(define xz (make-interval -2 1))
(define yz (make-interval -1 2))

(display (par1 xp yp)) (display (par2 xp yp)) (newline)
(display (par1 xp yz)) (display (par2 xp yz)) (newline)
(display (par1 xp ym)) (display (par2 xp ym)) (newline)
(display (par1 xz yp)) (display (par2 xz yp)) (newline)
(display (par1 xz yz)) (display (par2 xz yz)) (newline)
(display (par1 xz ym)) (display (par2 xz ym)) (newline)
(display (par1 xm yp)) (display (par2 xm yp)) (newline)
(display (par1 xm yz)) (display (par2 xm yz)) (newline)
(display (par1 xm ym)) (display (par2 xm ym)) (newline)

(newline)(newline)
(display "A/A") (newline)
(display (par1 xm xm)) (display (par2 xm xm)) (newline)
(newline)(newline)
(display "percent") (newline)
(define cp-x (make-center-percent (center xp) 0.50))
(define cp-y (make-center-percent (center yp) 0.50))
(display (par1 cp-x cp-y)) (display (par2 cp-x cp-y)) (newline)
(newline)(newline)

; answer

(define a (make-center-percent 100 1))
(define b (make-center-percent 200 1))
(percent a) 
(percent b) 
(define q0 (div-interval a a))
(percent q0) 
(define q1 (div-interval a b))
(percent q1) 
(define one (make-center-percent 1 0))

(define p (mul-interval a b))
(define s (add-interval a b))
(percent p) 
(exact->inexact (percent p)) 
(percent s) 
(percent (div-interval p s)) 

(define r0 (div-interval one a))
(percent r0) 
(define r1 (div-interval one b))
(percent r1) 
(define s0 (add-interval r0 r1))
(percent s0) 
(define r (div-interval one s0))
(percent r) 
