#lang racket


(define (element-of-set? x set)
	(cond ((null? set) false)
		  ((equal? x (car set)) true)
		  (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) '())
		  ((element-of-set? (car set1) set2)
		  (cons (car set1) (intersection-set (cdr set1) set2)))
		  (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
	(cond ((and (null? set1) (null? set2)) '())
		  ((not (null? set1))
		  	(if (element-of-set? (car set1) set2)
				(union-set (cdr set1) set2)
				(cons (car set1) (union-set (cdr set1) set2))
			)
		  )
		  (else (cons (car set2) (union-set set1 (cdr set2)))))
)

; 別解
; https://sicp.iijlab.net/solution/ex2.3.html
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

(intersection-set '(a b c) '(a c))
(union-set '(a b c) '(c a d))
