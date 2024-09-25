#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
			result-list
			(copy-to-list
				(left-branch tree)
				(cons (entry tree) (copy-to-list (right-branch tree) result-list )))))
	(copy-to-list tree '()))

(define (union-set set1 set2)
	(define (union-set-list set1 set2)
		(cond
			((null? set1) set2)
			((null? set2) set1)
			((= (car set1) (car set2)) (union-set-list (cdr set1) set2))
			((> (car set1) (car set2)) (cons (car set2) (union-set-list set1 (cdr set2))))
			((< (car set1) (car set2)) (cons (car set1) (union-set-list (cdr set1) set2)))))
	(union-set-list (tree->list-2 set1) (tree->list-2 set2))
	; O(n) + O(n) = O(n)
)

(define (intersection-set set1 set2)
	(define (intersection-set-list set1 set2)
		(if (or (null? set1) (null? set2))
			'()
			(let ((x1 (car set1)) (x2 (car set2)))
				(cond
					((= x1 x2)
						(cons x1 (intersection-set (cdr set1) (cdr set2))))
					((< x1 x2)
						(intersection-set (cdr set1) set2))
					((< x2 x1)
						(intersection-set set1 (cdr set2)))))))
	(intersection-set-list (tree->list-2 set1) (tree->list-2 set2))
)
