#lang racket

(define (make-mobile left right)
	(list left right))

(define (make-branch length structure)
	(list length structure))

; ## a
; selector
; (define (left-branch mobile)
; 	(car mobile)
; )
; (define (right-branch mobile)
; 	(car (cdr mobile))
; )

; (define (branch-length branch)
; 	(car branch)
; )
; (define (branch-structure branch)
; 	(car (cdr branch))
; )

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))
; ##

; (define (total-weight mobile)
; 	(cond 
; 		((null? mobile) 0)
; 		((not (pair? mobile)) mobile)
; 		(else
; 			; (pair? mobile)
; 			(+ (total-weight (branch-structure (left-branch mobile)))
; 			 	(total-weight (branch-structure (right-branch mobile)))
; 			)
; 		)
; 	)
; )

(define (total-weight mobile)
(if (not (pair? mobile)) mobile
 (let ((left-b (left-branch mobile))
       (right-b (right-branch mobile)))
   (let ((left-s (branch-structure left-b))
         (right-s (branch-structure right-b)))
     (+ (total-weight left-s)
        (total-weight right-s))))))

(define x (list (list 1 2) (list 3 4)))
(define y (list 10))
(define mobile (make-mobile x y))

; (mobile)
(total-weight mobile)
(define mymobile '((6 ((1 2) (2 1))) (3 ((2 4) (4 2)))))

(total-weight mymobile) 
