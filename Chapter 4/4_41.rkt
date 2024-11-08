#lang sicp
(#%require (only racket/base current-seconds))

(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (random range))))
(define (duplicate? n without-numbers)
	(if (null? without-numbers)
		#f
		(or (= n (car without-numbers)) (duplicate? n (cdr without-numbers)))))
; randomじゃなくて(1 2 3 4 5)から取るほうがいい
(define (random-in-5 without-numbers) 
	(let ((n (random-in-range 1 6))) ; until 5
		(if (duplicate? n without-numbers)
			(random-in-5 without-numbers)
			n)))

; (random-in-5 '(1 2 3 4))

(define (multiple-dwelling)
	(let* ((baker		(random-in-5 (list)))
			(cooper		(random-in-5 (list baker)))
			(fletcher	(random-in-5 (list baker cooper)))
			(miller		(random-in-5 (list baker cooper fletcher)))
			(smith		(random-in-5 (list baker cooper fletcher miller)))
		)
		(if (or (= baker 5)
				(= cooper 1)
				(= fletcher 5)
				(= fletcher 1)
				(<= miller cooper)
				(= (abs (- smith fletcher)) 1)
				(= (abs (- fletcher cooper)) 1))
			(multiple-dwelling)
			(list (list 'baker baker) (list 'cooper cooper)
				(list 'fletcher fletcher) (list 'miller miller)
				(list 'smith smith)))))

(multiple-dwelling)
