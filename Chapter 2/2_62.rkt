#lang racket

(define (element-of-set? x set)
	(cond ((null? set) false)
		((= x (car set)) true)
		((< x (car set)) false)
		(else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
	(cond
		((null? set1) set2)
		((null? set2) set1)
		((= (car set1) (car set2)) (union-set (cdr set1) set2))
		((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))
		((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))))

(union-set '(1 2 3) '(2 3 4 6 9))
(union-set '(2 3 4 6 9) '(1 2 3))
(union-set '() '(1 2))
(union-set '(1 2) '())
(union-set '() '())