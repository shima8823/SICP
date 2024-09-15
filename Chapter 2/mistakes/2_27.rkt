#lang racket

(define nil `())

(define (reverse l)
	(display "l : ")
	(display l)
	(newline)

	(if (null? l)
		(list)
		(append (reverse (cdr l)) (list (car l)))
	)
)

; (define (deep-reverse l)
; 	(cond
; 		((null? l)
; 			(display "nil")
; 			(newline)

; 			nil)
; 		((not (pair? l))
; 			(display "not pair?: ")
; 			(display l)
; 			(newline)

; 			(append nil (list l))
; 		)
; 		((and (null? (cdr l)) (not (pair? (car l))))
; 			(display "and (null? (cdr l)) (not (pair? (car l))): ")
; 			(display l)
; 			(newline)

; 			(append nil l)
; 		)


; 		; ((not (list? l))
; 		; 	(display "not pair: ")
; 		; 	(display l)
; 		; 	(newline)
; 		; 	(append nil (list l)))
; 		; 	; cdr = null & carがlistじゃないならば追加する
; 		; 	; !pairなら(list l)
; 		(else 
; 			(display "pair : ")
; 			(display l)
; 			(newline)
; 			(if (and (not (list? (car (cdr l)))) (not (list? (car l))))
; 				(list (append (deep-reverse (cdr l)) (deep-reverse (car l))))
; 				(append (deep-reverse (cdr l)) (deep-reverse (car l))))
; 			)
; 	)
; )

(define (ans-deep-reverse l)
	(if (pair? l)
		(append
			(ans-deep-reverse (cdr l))
			(list (ans-deep-reverse (car l)))
		)
		l
	)
)

(define x (list (list 1 2) (list 3 4)))
(define y (list 1 2 3 4))

; (deep-reverse x)
; (deep-reverse y)

; (pair? `(1 2 (1 2)))
; (list? `(1 2 (1 2)))

(ans-deep-reverse x)
(ans-deep-reverse y)

(append `(1 2 3 4) `((1 2)))