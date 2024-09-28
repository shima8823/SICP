#lang racket

#|

a: number?とvariable?はディスパッチとして取り込めない理由
登録できる型がないため。

d: ((get (operator exp) 'deriv) (operands exp) var)
ディスパッチとして取り組む時のput

|#

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
	(and (pair? x) (eq? (car x) '+)))
(define (addend s) (car s))
(define (augend s) (cadr s))
(define (product? x)
	(and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))
(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
		  ((=number? a2 0) a1)
		  ((and (number? a1) (number? a2)) (+ a1 a2))
		  (else (list '+ a1 a2))))
(define (=number? exp num)
	(and (number? exp) (= exp num)))
(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
		  ((=number? m1 1) m2)
		  ((=number? m2 1) m1)
		  ((and (number? m1) (number? m2)) (* m1 m2))
		  (else (list '* m1 m2))))

(define (exponentiation? x) ; (** u n)
	(and (pair? x) (eq? (car x) '**)))

(define (base e) (car e))
(define (exponent e) (cadr e))
(define (make-exponentiation b e)
	(cond ((=number? e 0) 1)
		  ((=number? e 1) b)
		  ((and (number? b) (number? e)) (* b (make-exponentiation b (- e 1))))
		  (else (list '** b e)))
)

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (install-calc-package)
	(define (sum exp var)
		(make-sum (deriv (addend exp) var)
				  (deriv (augend exp) var))
	)
	(define (mul exp var)
		(make-sum
				(make-product (multiplier exp)
							(deriv (multiplicand exp) var))
				(make-product (deriv (multiplier exp) var)
							(multiplicand exp)))
	)

	(define (exponentiation exp var)
		(make-product
			(make-product
				(exponent exp)
				(make-exponentiation (base exp) (- (exponent exp) 1))
			)
			(deriv (base exp) var)
		)
	)

	(put 'deriv '+ sum)
	(put 'deriv '* mul)
	(put 'deriv '** exponentiation)
	'done
)

(define (deriv exp var)
	(cond ((number? exp) 0)
		  ((variable? exp) (if (same-variable? exp var) 1 0))
		  (else ((get 'deriv (operator exp))
					(operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(install-calc-package)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(+ x (* 3 (+ x (+ y 2))))  'x)
(deriv '(+ (** x 3) (+ (** x 2) (+ x 1))) 'x) (deriv '(+ x x x) 'x) 
(deriv '(* x x x) 'x) 
(deriv '(+ x (* x  (+ x (+ y 2)))) 'x) 
(deriv '(** x 3) 'x) 