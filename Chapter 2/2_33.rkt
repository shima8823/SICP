#lang racket

(define nil `())
(define (square x) (* x x))

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence))
		)
	)
)

(define (map-self p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) nil sequence)
)

(define (append-self seq1 seq2)
	(accumulate cons seq2 seq1)
)

(define (length-self sequence)
	(accumulate (lambda (x y)  (+ 1 y)) 0 sequence)
)


(map-self square `(1 2 3 4))
(append-self `(1 2 3 4) `(5 6 7 8))
(length-self `(1 2 3 4 5 6 7 8))
