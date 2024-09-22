; 良問

#lang racket

(define (length items)
	(if (null? items)
		0
		(+ 1 (length (cdr items)))
	)
)

(define (deriv exp var)
	(define (augend s)
		(let ((len (length s)))
			(if (<= len 1)
				(car s)
				(if (<= (length (cddr s)) 1)
					(caddr s)
					(cddr s)))
		)
		
	)

	(display exp) (newline)
	(cond ((number? exp) 0)
		  ((variable? exp)
			(if (same-variable? exp var) 1 0))
		  ((sum? exp)
			(make-sum (deriv (addend exp) var)
					  (deriv (augend exp) var)
			)
		  )
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

; a change
(define (sum? x)
	(and (pair? x) (not (null? (cdr x))) (eq? (cadr x) '+)))
; a change
(define (addend s) (car s))
; a change
(define (product? x)
	(and (pair? x) (not (null? (cdr x))) (eq? (cadr x) '*)))
; a change
(define (multiplier p) (car p))
(define (multiplicand p)
	(caddr p)
)
(define (multiplier-b p) (car p))
(define (multiplicand-b p)
	(caddr p)
)

(define (make-sum a1 a2)
	(display "make-sum ")
	(display "a1: ") (display a1) (display " a2: ") (display a2) (newline)
	(cond ((=number? a1 0) a2)
		  ((=number? a2 0) a1)
		  ((and (number? a1) (number? a2)) (+ a1 a2))
		  (else (list '+ a1 a2))))
(define (=number? exp num)
	(and (number? exp) (= exp num)))
(define (make-product m1 m2)
	(display "make-product ")
	(display "m1: ") (display m1) (display " m2: ") (display m2) (newline)
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

(define (include-bracket? exp)
	(and (not (null? exp)) (or (pair? (car exp)) (include-bracket? (cdr exp))))
)
(define (include-product? exp)
	(display "include produc: ")
				(display exp) (newline)
	(and (not (null? exp)) (or (product? exp) (include-product? (cdr exp))))
)

(define (get-bracket exp)
	(if (pair? (car exp))
		(car exp)
		(get-bracket (cdr exp))
	)
)
(define (get-product exp)
	(if (product? exp)
		exp
		(get-product (cdr exp))
	)
)

(define (equal? a b)
	(or (and (not (pair? a)) (not (pair? b)) (eq? a b))
		(and (pair? a) (pair? b)
			(equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))

(define (make-without-product exp var product)
	(display "mk-wo-pro: ")
	(display exp) (newline)

	(cond
		((equal? exp product) 
			(append 
				(let ((sum 
						(make-sum
							(make-product (multiplier exp)
										(deriv (multiplicand exp) var))
							(make-product (deriv (multiplier exp) var)
										(multiplicand exp))
						))
					)
					(if (list? sum)
						(list (append sum '(make)))
						(list (append (list sum) '(make)))
					)
				)
				(cdddr exp)
			)
		)
		(else (append (list (car exp)) (make-without-product (cdr exp) var product)))
	)
)


; (display (deriv '(+ x (* 3 (+ x (+ y 2)))) 'x)) (newline)
; x + 3(x + y + 2)
; 4x + 3y + 6 -> 4
; a
; (display (deriv '(x + (3 * (x + (y + 2)))) 'x)) (newline)(newline)
; (display (deriv '(3 * (x + (y + 2))) 'x)) (newline) (newline) 

#|

b
priority sum < product
sumの被加数をderivしてあげる

|#
(display (deriv '(3 * x) 'x)) (newline) ; ->3になるべき
 (newline) 
(display (deriv '(3 * (x + y + 2)) 'x)) (newline) ; ->3になるべき
 (newline) 
(display (deriv '(x + 3 * (x + y + 2)) 'x)) (newline)
(display (deriv '(x + 3 * (x + y + 2) * (x + y + 2)) 'x)) (newline)

; 各パーツテスト
; (display (get-bracket '(x + 3 * (x + y + 2)))) (newline)
; (display (get-product '(x + 3 * 1))) (newline)
; (display (make-without-product '(x + 3 * 1) 'x '(3 * 1))) (newline)

; (display (deriv '(x + 3 + (x + y + 2)) 'x)) (newline)

