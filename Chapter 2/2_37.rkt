; 良問

#lang racket

(define nil `())

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence))
		)
	)
)

(define (accumulate-n op init seqs)
	; (display seqs)
	; (newline)
	(if (null? (car seqs))
		nil
		(cons
			(accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs))
		)
	)
)

(define (dot-product v w)
	(accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
	(map (lambda (mi) (dot-product v mi)) m))
(define (transpose mat)
	(accumulate-n
		(lambda (x y)
			; (display x)
			; (newline)
			(cons x y)
		)
		nil
		mat
	)
)
(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(
			map
			(lambda (x)
				; (1 2 3 4)
				; (4 5 6 6)
				; (6 7 8 9)

				; (1 4 6)
				; (2 5 7)
				; (3 6 8)
				; (4 6 9)
				; cols
				; (1 2 3 4)
				; (4 5 6 6)
				; (6 7 8 9)
				(map
					(lambda (y)
						; (display "x: ")
						; (display x)
						; (newline)
						; (display "y: ")
						; (display y)
						; (newline)
						; (newline)
						(dot-product x y)
					)
					cols
				)
			)
			m
		)
	)
)

(define x (list 1 2 3 4))
(define y (list 4 5 6 6))
(define z (list 6 7 8 9))
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(dot-product x y)
m
(matrix-*-vector m (list 1 2 3 4))
; '(30 56 80)
(transpose m)
#|
((1 2 3 4) (4 5 6 6) (6 7 8 9))
(1 2 3 4)
(4 5 6 6)
(6 7 8 9)

((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(1 4 6)
(2 5 7)
(3 6 8)
(4 6 9)

|#

(matrix-*-matrix m (transpose m))
