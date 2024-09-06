#lang racket

(define (sum term a next b)
	(if (> a b)
		0
		(+	(term a)
			(sum term (next a) next b)
		)
	)
)

(define (integral f a b dx)
	(define (add-dx x)
		(+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
)

(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (even? n)
	(= (remainder n 2) 0))

(define (sympthon-formula f a b n)
	(define h
		(/ (- b a) n)
	)

	(define (yk k)
		(define (y)
			(f (+ a (* k h)))
		)
		
		(cond	((or (= k 0) (= k n)) 
					(y))
				((even? k) ;even
					(* 2 (y)))
				(else ;odd
					(* 4 (y)))
		)
	)

	(* (sum yk a inc n) (/ h 3))
)

(integral cube 0 1 0.01)
(sympthon-formula cube 0 1.0 100)

; sympthonの方が正確である。