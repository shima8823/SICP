#lang racket

(define nil `())
(define (square x) (* x x))

(define (tree-map proc tree)
	(map
		(lambda (sub-tree)
			(if (pair? sub-tree)
				(square-tree sub-tree)
				(proc sub-tree)
			)
		)
		tree
	)
)

(define (square-tree tree) (tree-map square tree))

(define lx (list 1(list 2 (list 3 4) 5)(list 6 7)))
lx
(square-tree lx)
