#lang racket
(require r5rs) ; set-(car|cdr)!

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (adjoin-set x set) ;change
	(cond
		((null? set) (make-tree x '() '()))
		((= (get-key x) (get-key (entry set))) set)
		((< (get-key x) (get-key (entry set)))
			(make-tree	(entry set)
				(adjoin-set x (left-branch set)) (right-branch set)))
		((> (get-key x) (get-key (entry set)))
			(make-tree	(entry set)
				(left-branch set) (adjoin-set x (right-branch set))))))

(define (make-table) (list '*table*))
(define (get-key pair) (car pair)) ;add

(define (lookup key table)
	(let ((record (assoc key (cdr table))))
		(if record
			(cdr record)
			false)))
; 
(define (assoc key records) ;change ;like element-of-set
	(cond ;2分探索
		((null? records) false)
		((equal? key (get-key (entry records))) (entry records))
		((< key (get-key (entry records))) (assoc key (left-branch records)))
		((> key (get-key (entry records))) (assoc key (right-branch records)))))
(define (element-of-set? x set)
	(cond ((null? set) false)
		((= x (entry set)) true)
		((< x (entry set)) (element-of-set? x (left-branch set)))
		((> x (entry set)) (element-of-set? x (right-branch set)))))
; 

(define (insert! key value table)
	(let ((record (assoc key (cdr table))))
		(if record
			(set-cdr! record value)
			(set-cdr! table (adjoin-set (cons key value) (cdr table))) ; change
			))
	'ok)

(define table (make-table))
(insert! 4 'd table)
(insert! 1 'a table)
(insert! 2 'b table)
(insert! 3 'c table)
(insert! 6 'f table)
(insert! 5 'e table)
(insert! 7 'g table)

;  {*table* {4 . d} {{1 . a} () {{2 . b} () {{3 . c} () ()}}} {{6 . f} {{5 . e} () ()} {{7 . g} () ()}}}

(lookup 4 table)
(lookup 7 table)
(lookup 1 table)