#lang racket

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x) (set-car! (car x) 'wow) x)

z1 ;((a b) a b)
(set-to-wow! z1) ;((wow b) wow b)
z2 ;((a b) a b)
(set-to-wow! z2) ;((wow b) a b)

#|
z1 -> [b1 b2]
	  ↓  ↓
x -> [b1 b2] b2-> [bb1 bb2]
	↓
	b1='wow


z2 -> [b1 b2] b2-> [bb1 bb2] bb2-> [bbb1 /]
	  ↓				↓				↓
	  ↓			  'a				'b
	  ↓								⇧
	  b1 -> 	   [bb1 bb2] bb2-> [bbb1 /]
					↓
					'wow
|#