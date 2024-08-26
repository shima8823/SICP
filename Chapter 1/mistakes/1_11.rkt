#lang racket

(define (f_rec n) 
	(cond	((< n 3) n)
			(else (+	(f_rec (- n 1))
						(* 2 (f_rec (- n 2)))
						(* 3 (f_rec (- n 3))))))
)

(f_rec 5)
#|
	(f 4) + 2(f 3) + 3(f 2) = 11 + 8 + 6 = 25
		(f 4)
			(f 3) + 2(f 2) + 3(f 1)
				4 + 4 + 3 = 11

	(f 3)
		(f 2) + 2(f 1) + 3(f 0)
			2 + 2 + 0 = 4
|#

; (define (f n) 
; 	(define f_iter a b c counter)
; 		(if (< n 3)
; 			n
; 			(f_iter ()))
; 	(f_iter )
; )
; (f_iter 5)

; iter answer
(define (fi n) 
	(define (f_iter a b c n)
		(cond	((= n 0) c)
				((= n 1) b)
				((= n 2) a)
				(else (f_iter (+ a b b c c c) a b (- n 1)))))
	(f_iter 2 1 0 n)
)
(fi 5)

#|
(f_iter 2 1 0 5)
	(f_iter 4 2 1 4)
		(f_iter 11 4 2 3)
			(f_iter 25 11 4 2)
				25
|#
