#lang sicp

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
	(let ((baker (amb 1 2 3 4 5)) (cooper (amb 1 2 3 4 5)) (fletcher (amb 1 2 3 4 5)) (miller (amb 1 2 3 4 5)) (smith (amb 1 2 3 4 5)))
		(require (distinct? (list baker cooper fletcher miller smith)))
		(require (not (= baker 5)))
		(require (not (= cooper 1)))
		(require (not (= fletcher 5)))
		(require (not (= fletcher 1)))
		(require (> miller cooper))
		; (require (not (= (abs (- smith fletcher)) 1)))
		(require (not (= (abs (- fletcher cooper)) 1)))
		(list (list 'baker baker) (list 'cooper cooper)
			(list 'fletcher fletcher) (list 'miller miller)
			(list 'smith smith))))

; amb-interpreter.rktでexecute
; 5ko
; 1 2 3 4 5
;  f    c  
;  f    c  
;  c    f  
;  c    f  