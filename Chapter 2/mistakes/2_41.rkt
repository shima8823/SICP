; 良問

#lang racket

(define nil `())
(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))
(define (prime? n)
	(= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
		(cond	((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (+ test-divisor 1)))
		)
)

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

(define (flatmap proc seq)
	(accumulate append nil (map proc seq)))

(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
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

(define (s-equal-sum n s)
	(define (same-s-sum? pair)
		(display "car: ")(display (car pair))
		(display " cadr: ")(display (cadr pair))
		(display " caddr: ")(display (caddr pair))
		(newline)
		(= s (+ (car pair) (cadr pair) (caddr pair)))
	)
	(filter
		same-s-sum?
		(flatmap
			(lambda (i)
				(map
					(lambda (j)
					; 	(map 
					; 		(lambda (k) (list k j i))
					; 		(enumerate-interval 1 (- j 1))
					; 	)
					; )
					; (enumerate-interval 1 (- i 1))
					; ここがわからなかった。
						(cons i j)
					)
					(unique-pairs (- i 1))
				)
			)
			(enumerate-interval 1 n)
		)
	)
)

(display "s-equal-sum")
(newline)
(s-equal-sum 10 15)
(newline)
(flatmap
	(lambda (i)
		(map 
			(lambda (j) (display "ko")(newline)(list i j))
			(enumerate-interval 1 (- i 1))
		)
	)
	(enumerate-interval 1 2)
)
#|

flatmap2
im = (1 2)
i = 1
jm = ()
i = 2
jm = (1)
j = 1
(2 1)

flatmap3
im = (1 2 3)
i = 1
jm = ()
i = 2
jm = (1)
km = ()
i = 3
jm = (1 2)
j = 1
km = ()
j = 2
km = (1)
k = 1
(3 2 1)




|#

(display "3 flatmap")
(newline)
(flatmap
	(lambda (i)
		(map
			(lambda (j)
				(map 
					(lambda (k) (display "ok")(newline)(list i j k))
					(enumerate-interval 1 (- j 1))
				)
				
			)
			(enumerate-interval 1 (- i 1))
		)
	)
	(enumerate-interval 1 5)
)

(display "filter 3 flatmap")
(newline)
(filter (lambda (x) (not (null? x)))
(map (lambda (l) (if (pair? l) (car l) l))
(flatmap
	(lambda (i)
		(map
			(lambda (j)
				(map 
					(lambda (k) (display "ok")(newline)(list i j k))
					(enumerate-interval 1 (- j 1))
				)
			)
			(enumerate-interval 1 (- i 1))
		)
	)
	(enumerate-interval 1 5)
)
)
)

; (append nil nil)

(append nil (list nil))
