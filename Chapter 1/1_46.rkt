#lang racket

(define tolerance 0.00001)
(define (abs x)
	(cond	((< x 0) (- x))
			(else x)
	)
)
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (iterative-improve close-enough? improve)
	(define (iter guess)
		(let ((next (improve guess)))
			(if (close-enough? guess next)
				next
				(iter next)
			)
		)
	)
	(lambda (guess)
		(iter guess)
	)
)

(define (sqrt x)
	(define (improve guess)
		(average guess (/ x guess)))
	(define (good-enough? guess next)
		(< (abs (- (square guess) x)) 0.001))
	; (define (sqrt-iter guess x) 
	; 	(if (good-enough? guess x)
	; 		guess
	; 		(sqrt-iter (improve guess x) x)
	; 	)
	; )

	((iterative-improve good-enough? improve) 1.0)
	; (define (sqrt-iter guess x) 
	; 	(if (good-enough? guess x)
	; 		guess
	; 		(sqrt-iter (improve guess) x))
	; )
	; (sqrt-iter 1.0 x)

)

(sqrt 1)
(sqrt 2)
(sqrt 9)
(sqrt 625)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance)
	)
	; (define (try guess)
	; 	(let ((next (f guess)))
	; 	(if (close-enough? guess next)
	; 		next
	; 		(try next)))
	; )
	((iterative-improve close-enough? f) first-guess)
)

(fixed-point cos 1.0) ;.7390822985224023
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0) ;1.2587315962971173
