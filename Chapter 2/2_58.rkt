#lang racket

(define (deriv exp var)
	(cond ((number? exp) 0)
		  ((variable? exp)
			(if (same-variable? exp var) 1 0))
		  ; ()を探す 
		;   ((include-bracket? exp)
		;   	; (deriv (元の式 (()の式deriv)))
		; 	(deriv 
		; 		(map
		; 			(lambda (x)
		; 				(if (eq? (get-bracket exp) x)
		; 					(deriv (get-bracket exp) var)
		; 					x
		; 				)
		; 			)
		; 			exp
		; 		)
		; 		var
		; 	)
		;   )
		; ;   *を探す
		;   ((include-product? exp)
		;   	; (deriv (元の式 (()の式deriv)))
		; 	(deriv 
		; 		(make-without-product exp var (get-product exp))
		; 		var
		; 	)
		;   )
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

; a change
(define (sum? x)
	(and (pair? x) (eq? (cadr x) '+)))
; a change
(define (addend s) (car s))
(define (augend s)
	(caddr s)
)
; a change
(define (product? x)
	(and (pair? x) (eq? (cadr x) '*)))
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

(define (include-bracket? exp)
	(and (not (null? exp)) (or (pair? (car exp)) (include-bracket? (cdr exp))))
)
(define (include-product? exp)
	(and (not (null? exp)) (or (product? (car exp)) (include-product? (cdr exp))))
)

(define (get-bracket exp)
	(if (pair? (car exp))
		(car exp)
		(get-bracket (cdr exp))
	)
)
(define (get-product exp)
	(if (product? (cadr exp))
		(car exp)
		(get-product (cdr exp))
	)
)

(define (equal? a b)
	(or (and (not (pair? a)) (not (pair? b)) (eq? a b))
		(and (pair? a) (pair? b)
			(equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))

(define (make-without-product exp var product)
	(cond
		((equal? exp product) 
			(append 
				(make-sum
					(make-product (multiplier exp)
								(deriv (multiplicand exp) var))
					(make-product (deriv (multiplier exp) var)
								(multiplicand exp))
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
(display (deriv '(x + (3 * (x + (y + 2)))) 'x)) (newline)

#|

b
全体から括弧の中身を計算
全体から掛け算を計算
全体から足し算を計算

|#
; なんで1？
(display (deriv '(x + 3 * (x + y + 2)) 'x)) (newline)
(display (deriv '(x + y + 2) 'x)) (newline) ; 1
(display (get-bracket '(x + 3 * (x + y + 2)))) (newline)
; (display (get-product '(x + 3 * 1))) (newline)
(display (make-without-product '(x + 3 * 1) 'x '(3 * 1))) (newline)


; (display (deriv '(x + 3 + (x + y + 2)) 'x)) (newline)

