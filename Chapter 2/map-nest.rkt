#lang racket

(define nil `())
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
		(cond	((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (+ test-divisor 1)))
		)
)

(define (prime? n)
	(= n (smallest-divisor n)))

(define (enumerate-interval low high)
	(if (> low high)
		nil
		(cons low (enumerate-interval (+ low 1) high))
	)
)

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence))
		)
	)
)

(define n 3)

; (enumerate-interval 1 n)
(accumulate
	append
	nil
	(map
		(lambda (i)
		; (display i)(newline)
			(map (lambda (j)
				; (display "j: ")(display j)
				; (newline)
				(list i j))
				(enumerate-interval 1 (- i 1))
			)
		)
		(enumerate-interval 1 n) ; (1 2 3)
	)
)

(define (flatmap proc seq)
	(accumulate append nil (map proc seq)))

(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter prime-sum?
			(flatmap
				(lambda (i)
					(map 
						(lambda (j) (list i j))
						(enumerate-interval 1 (- i 1))
					)
				)
				(enumerate-interval 1 n)
			)
		)
	)
)

(flatmap (lambda (x) (list (square x))) (enumerate-interval 1 5))
(prime-sum-pairs 6)

(define (permutations s)
	(if (null? s)	; 集合は空か?
		(list nil)	; 空集合を持つ列
		(flatmap
			(lambda (x)
				(map (lambda (p) (cons x p))
					(permutations (remove x s))))
			s
		)
	)
)

(define (remove item sequence)
	(filter
		(lambda (x) (not (= x item)))
		sequence)
)

(permutations (list 1 2 3))