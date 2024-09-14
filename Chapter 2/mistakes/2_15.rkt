; わからん...

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

;par1には不確かな変数が4回, par2には2回現れる. 実験してみると

(define r1 (make-center-percent 200 5))
(define r2 (make-center-percent 300 5))

(define p1 (par1 r1 r2)) ;par1で計算した結果をp1
(define p2 (par2 r1 r2)) ;par2で計算した結果をp2とする 正確な並列抵抗は120

p1 ;==> (111.29268292682927 . 129.30769230769232)

p2 ;==> (116.99999999999999 . 123.00000000000001)

; 変数というのはp1_l, p1_u, center, percentのこと？
(center p1) ;==> 120.3001876172608

(percent p1) ;==> 14.975046787273868

(center p2) ;==> 120.

(percent p2) ;==> 5.000000000000024