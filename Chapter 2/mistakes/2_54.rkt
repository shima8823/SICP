; 良問
; 数字のテストも必要だった

#lang racket

(define (memq item x)
	(cond ((null? x) false)
		  ((eq? item (car x)) x)
		  (else (memq item (cdr x)))))

; (define (equal? a b)
; 	(cond ((or
; 			(and (null? a) (not (null? b)))
; 			(and (null? b) (not (null? a))))
; 			#f)
; 		  ((and (null? a) (null? b)) #t)
; 		  (else (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b)))))
; )

(define (equal? a b)
	(or (and (not (pair? a)) (not (pair? b)) (eq? a b))
		(and (pair? a) (pair? b)
			(equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this is a ))
(equal? 4 4)
(equal? '(this is a list) '(this (is a) list))

