#lang racket

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))
	)
)

(define (make-rat n d) ; 有理数(make rational number)
	; 両方マイナスの場合はgcdもマイナスの返り値なので何もしない
	; 片方にマイナスがついていた場合分子にマイナスをつけて
	(define minus-side?
		(or (and (< n 0) (>= d 0)) (and (>= n 0) (< d 0)))
	)

	(let
		(
			(g
				(if minus-side?
					(- (gcd n d))
					(gcd n d)
				)
			)
		)
		(cons (/ n g) (/ d g))
	)
)
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x))
)

(define (add-rat x y)
	(make-rat
		(+ (* (numer x) (denom y))
			(* (numer y) (denom x)))
		(* (denom x) (denom y))
	)
)

(define (sub-rat x y)
	(make-rat
		(- (* (numer x) (denom y))
			(* (numer y) (denom x)))
		(* (denom x) (denom y))
	)
)

(define (mul-rat x y) 
	(make-rat
		(* (numer x) (numer y))
		(* (denom x) (denom y))
	)
)

(define (div-rat x y)
	(make-rat
		(* (numer x) (denom y))
		(* (denom x) (numer y))
	)
)

(define (equal-rat? x y)
	(= (* (numer x) (denom y))
		(* (numer y) (denom x))
	)
)

; (define one-third (make-rat 1 3))
; (print-rat (add-rat one-third one-third))
(define a (make-rat 1 3))
(define b (make-rat -1 3))
(define c (make-rat 1 -3))
(define d (make-rat -1 -3))
(print-rat a)
(print-rat b)
(print-rat c)
(print-rat d)