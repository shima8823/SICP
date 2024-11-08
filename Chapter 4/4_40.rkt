#lang sicp

(#%require (only racket/base time)) ; import time

(define (require p) (if (not p) (amb)))
(define (an-element-of items)
	(require (not (null? items)))
	(amb (car items) (an-element-of (cdr items))))

(define (distinct? items)
	(cond ((null? items) true)
		  ((null? (cdr items)) true)
		  ((member (car items) (cdr items)) false)
		  (else (distinct? (cdr items)))))

(define (multiple-dwelling)
	(let ((fletcher (amb 1 2 3 4 5)))
		(require (not (= fletcher 5)))
		(require (not (= fletcher 1)))
		(let ((cooper (amb 1 2 3 4 5)))
			(require (not (= (abs (- fletcher cooper)) 1)))
			(require (not (= cooper 1)))
			(let ((smith (amb 1 2 3 4 5)))
				(require (not (= (abs (- smith fletcher)) 1)))
				(let ((miller (amb 1 2 3 4 5)))
					(require (> miller cooper))
					(let ((baker (amb 1 2 3 4 5)))
					 	(require (not (= baker 5)))
						(require (distinct? (list baker cooper fletcher miller smith)))
						(list (list 'baker baker) (list 'cooper cooper)
							(list 'fletcher fletcher) (list 'miller miller)
							(list 'smith smith))))))))

(multiple-dwelling)

#|

階の割り当てがそれぞれ別々でなければならないという制約を設定する前と後で、それぞれ何組あるだろうか。

前
n^n

後
n * n-1 * n-2 * n-3 ... n-(n-1)
.'. n!

|#