#lang racket

(define tolerance 0.00001)
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (average x y)
	(/ (+ x y) 2.0)
)
(define (average-damp f)
	(lambda (x) (average x (f x)))
)
; (lambda (y) (average y (/ x (* y y y))))

(define (compose f g)
	(lambda (x)
		(f (g x))
	)
)

(define (repeated f n)
	(if (= n 0)
		(lambda (x) x)
		(compose (repeated f (- n 1)) f)
	)
)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2) 
		(< (abs (- v1 v2))
			tolerance))
	(define (try guess)
		(let ((next (f guess)))
		(if (close-enough? guess next)
			next
			(try next)))
	)
	(try first-guess)
)

(define (sqrt x)
	(fixed-point 
		(lambda (y) (average y (/ x y)))
		1.0
	)
)

(define (cube-root x)
	(fixed-point 
		(average-damp (lambda (y) (/ x (square y))))
		1.0
	)
)


(define (fourth-root x)
	(fixed-point 
		(
			(repeated
				average-damp
				2
			)
			(lambda (y) (/ x (expt y 3)))
		)
		1.0
	)
)

(define (fifth-root x)
	(fixed-point 
		(
			(repeated
				average-damp
				2
			)
			(lambda (y) (/ x (expt y 4)))
		)
		1.0
	)
)

(define (sixth-root x)
	(fixed-point 
		(
			(repeated
				average-damp
				2
			)
			(lambda (y) (/ x (expt y 5)))
		)
		1.0
	)
)

(define (seventh-root x)
	(fixed-point 
		(
			(repeated
				average-damp
				2
			)
			(lambda (y) (/ x (expt y 6)))
		)
		1.0
	)
)

(define (eighth-root x)
	;以下はそもそも数式が違うため別の値になってしまう
	; (fixed-point 
	; 	(repeated
	; 		(lambda (y) (average y (/ x (expt y 7))))
	; 		2
	; 	)
	; 	1.0
	; )
	(fixed-point
		(
			(repeated average-damp 3) 
			(lambda (y) (/ x (expt y (- 8 1))))
		)
		1.0
	)
)

(define (n-root n x)
	; k = 2をk乗するとnになる
	(let ((k (floor (/ (log n) (log 2)))))
		(fixed-point
			(
				(repeated average-damp k)
				(lambda (y) (/ x (expt y (- 8 1))))
			)
			1.0
		)
	)
)

;平均緩和数
(sqrt 4) ;1
(cube-root 8) ;1
(fourth-root 16) ;2
(fifth-root 32) ;2
(sixth-root 64) ;2
(seventh-root 128) ;2
(eighth-root 256) ;2
(n-root 8 256) ;2

; ((average-damp square) 10)
; ((lambda (y) (average y (square y))) 10)

; (repeated
; 	(lambda (y) (average y (/ 256 (expt y 7))))
; 	2
; )
; f(f(x))
; (lambda (y) (average y (/ 256 (expt y 7))))((lambda (y) (average y (/ 256 (expt y 7)))))


; (lambda (x) (average x (f x)))
; average-damp(average-damp((lambda (y) (/ 256 (expt y 7)))))
; (lambda (x) (average x (f x)))((lambda (x) (average x (f x)))((lambda (y) (/ 256 (expt y 7)))))

