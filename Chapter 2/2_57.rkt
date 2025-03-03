#lang racket

(define (deriv exp var)
	(cond ((number? exp) 0)
		  ((variable? exp)
			(if (same-variable? exp var) 1 0))
		  ((sum? exp)
			(make-sum (deriv (addend exp) var)
					  (deriv (augend exp) var)))
		  ((product? exp)
			(make-sum
				(make-product (multiplier exp)
							(deriv (multiplicand exp) var))
				(make-product (deriv (multiplier exp) var)
							(multiplicand exp))))
		  ((exponentiation? exp) ; (** u n) -> (n*u) ** (n-1) * du
			(make-product
				(make-product
					(exponent exp)
					(make-exponentiation (base exp) (- (exponent exp) 1))
				)
				(deriv (base exp) var)
			))
		  (else
			(error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
	(and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
; 追加
(define (augend s)
	(cond ((null? (cddr s)) 0)
		  (else 
			(make-sum
				(caddr s)
				(augend (cdr s)))))
)
(define (product? x)
	(and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
; 追加
(define (multiplicand p)
	; (* a b c d) -> (mk-pr b (mk-pr c (mk-pr d )))
	(cond ((null? (cddr p)) 1)
		  (else 
			(make-product
				(caddr p)
				(multiplicand (cdr p)))))
)
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

(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation b e)
	(cond ((=number? e 0) 1)
		  ((=number? e 1) b)
		  ((and (number? b) (number? e)) (* b (make-exponentiation b (- e 1))))
		  (else (list '** b e)))
)

; (display (deriv '(+ x 3) 'x)) (newline)
; (display (deriv '(* x y) 'x)) (newline)
(display (deriv '(* (* x y) (+ x 3)) 'x)) (newline)
(display (deriv '(* x y (+ x 3)) 'x)) (newline)
