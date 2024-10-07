#lang racket

(define (make-monitored f)
	(define (monitored count)
		(define (dispatch m)
				(cond ((eq? m 'how-many-calls?) count)
					((eq? m 'reset-count) (set! count 0))
					(else 
						(begin (set! count (+ count 1))
								(f m)))))
		dispatch)
	
	(monitored 0))

(define s (make-monitored sqrt))
(s 100)
; 10
(s 'how-many-calls?)
; 1

(s 100)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)

