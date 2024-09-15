#lang racket

#|

x = (1 3 (5 7) 9)
(car (cdr (car (cdr (cdr x)))))

x = ((7))
(car (car x))

x = (1 (2 (3 (4 (5 (6 7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ca))))))))))))

|#

(define a1 (list 5 7))
(define aa (list 1 3 a1 9))
(define b1 (list 7))
(define ba (list b1))
(define ca (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))


aa
ba
ca
(car (cdr (car (cdr (cdr aa)))))
(car (car ba))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ca))))))))))))