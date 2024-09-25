#lang racket

(define (adjoin-set x set)
	(cond
		((null? set) (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set))
			(make-tree	(entry set)
				(adjoin-set x (left-branch set)) (right-branch set)))
		((> x (entry set))
			(make-tree	(entry set)
				(left-branch set) (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
	(if (null? tree)
		'()
		(append (tree->list-1 (left-branch tree))
				(cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
			result-list
			(copy-to-list
				(left-branch tree)
				(cons (entry tree) (copy-to-list (right-branch tree) result-list )))))
	(copy-to-list tree '()))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))
(define tree (make-tree 7 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())) (make-tree 9 '() (make-tree 11 '() '()))))
(define tree2 (make-tree 3 (make-tree 1 '() '()) (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))))
(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 '() '()) '()) (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))
(define tree-quote '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

#|

a
同じ。'(1 3 5 7 9 11)
一番小さい木 '(1 3 5) '(1 3) '(3 5)で考える

b
tree->list-1の方が2に比べてO(logN)分遅い
append 左からまとめて最終的に全体のtree分の半分をcdrするため。

|#

(tree->list-1 tree)
(tree->list-2 tree)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)
(tree->list-2 tree-quote)
(tree->list-2 '(3 (1 () ()) (5 () ())))