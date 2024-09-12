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

(define (n-root n x)
	(let ((k (floor (/ (log n) (log 2)))))
		(fixed-point
			(
				(repeated average-damp k)
				(lambda (y) (/ x (expt y (- n 1))))
			)
			1.0
		)
	)
)

(define (cons a b) 
	(* (expt 2 a) (expt 3 b))
)

#|
a = 2
b = 3

4 * 27 = 108

108 / 3 = 36
36 / 3 = 12
12 / 3 = 4
4 / 2 = 2 a = 2?

a = 5
b = 3

32 * 27 = 864
864 / 3 / 3 / 3  = 32
32 / 2 


|#

(define (car i)
	(if (= (remainder i 3) 0)
		(car (/ i 3))
		(log i 2)
	)
)

(define (cdr i)
	(if (= (remainder i 2) 0)
		(cdr (/ i 2))
		(log i 3)
	)
)

(car (cons 5 3))
(cdr (cons 5 3))

; other answer
(define (pcar z)
  (if (= (remainder z 2) 0)
      (+ 1 (pcar (/ z 2)))
      0))

(define (pcdr z)
  (if (= (remainder z 3) 0)
      (+ 1 (pcdr (/ z 3)))
      0))

(pcar (cons 5 3))
(pcdr (cons 5 3))

; ##