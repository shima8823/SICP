; 説明が不明瞭な問題

#lang racket

; answer?
(define f
 (let ((x 0))
   (lambda (n)
     (if (= x 0) (begin (set! x 1) n) 0))))

(define (make-f)
	(define order 1)
	(lambda (x)
		(if (= order 0) 0 x)))

(define f (make-f))


