; 惜しい 足し算だけミスった😎
#lang racket

(define zero
	(lambda (f)
		(lambda (x) x)
	)
)

(define (add-1 n)
	(lambda (f)
		(lambda (x)
			(f ((n f) x))
		)
	)
)


; ((zero add-1) 10)

; (((add-1 zero) zero) 10)
; 足すということは関数を1個適用すること？

(define one
	(lambda (f)
		(lambda (x)
			(f x)
		)
	)
)

; oneにadd-1
(define two
	(lambda (f)
		(lambda (x)
			(f (f x))
		)
	)
)

; 
; (define (church+ a b)
; 	(lambda (f)
; 		(lambda (x)
; 			(f (f x))
; 		)
; 	)
; 	(cond	((= a b) a)
; 			((< a b) 
; 				(church+	(lambda (f)
; 						(lambda (x)
; 							(f a)
; 						)
; 					)
; 					b
; 				)
; 			)
; 			((> a b)
; 				(church+	a
; 					(lambda (f)
; 						(lambda (x)
; 							(f b)
; 						)
; 					)
; 				)
; 			)
; 	)
; )

(define (church+ m n) ; 関数だから適用させるだけで足し算になる
	(lambda (f) (lambda (x) ((m f) ((n f) x)))))


(define (inc n) (+ n 1))

((zero inc) 0)

((one inc) 0)

((two inc) 0)
(define four (church+ two two))

((four inc) 0)