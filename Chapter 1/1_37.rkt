#lang racket

; φ = 1.6180327868852458
; 1/φ = 0.6180327868852458
(define (cont-frac n d k)
	(if (= k 0)
		(/ (n k) (d k))
		(/ (n k) (+ (d k) (cont-frac n d (- k 1))))
	)
)

(cont-frac	(lambda (i) 1.0)
			(lambda (i) 1.0)
			10
)

; a
; 0.6180339887498948
; 小数点以下4桁の精度の近似を得るにはk >= 10でなければならない

; b
(define (cont-frac-iter n d k)
	(define (iter i result)
		(if (= i k)
			result
			(iter (+ i 1) (/ (n i) (+ (d i) result)))
		)
	)
	(iter 1 (/ (n 1) (d 1)))

	; (if (= k 0)
	; 	(/ (n k) (d k))
	; 	(/ (n k) (+ (d k) (cont-frac n d (- k 1))))
	; )
)

(cont-frac-iter	(lambda (i) 1.0)
				(lambda (i) 1.0)
				10
)
