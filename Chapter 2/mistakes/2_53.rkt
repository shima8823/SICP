#lang racket

(define (memq item x)
	(cond ((null? x) false)
		  ((eq? item (car x)) x)
		  (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(pear apple prune))
(memq 'apple '(x (apple sauce) y apple pear))
'a

(list 'a 'b 'c)
; ('a 'b 'c) -> '(a b c)
(list (list 'george))
; (('george))
(cdr '((x1 x2) (y1 y2)))
; ((y1 y2))
(cadr '((x1 x2) (y1 y2)))
; (y1 y2)
(pair? (car '(a short list)))
; #f
(memq 'red '((red shoes) (blue socks)))
; (red shoes) -> #f
(memq 'red '(red shoes blue socks))
; (red shoes blue socks)