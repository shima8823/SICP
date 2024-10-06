#lang racket

; ##### generic-calc
(define (square x) (* x x))

(define (round-nop x) ;nop = no point
	(inexact->exact (round x)))
(define (equal? a b)
	(or (and (not (pair? a)) (not (pair? b)) (eq? a b))
		(and (pair? a) (pair? b)
			(equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))

(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (attach-tag type-tag contents)
	(cond
		((or (eq? type-tag 'scheme-number) (eq? type-tag 'real-number))
			contents)
		(else (cons type-tag contents))))
(define (type-tag datum)
	(cond ((pair? datum) (car datum))
		  ((inexact? datum) 'real-number)
		  ((number? datum) 'scheme-number)
		  (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
	(cond ((pair? datum) (cdr datum))
		  ((or (number? datum) (inexact? datum)) datum)
		  (else (error "Bad tagged datum: CONTENTS" datum))))

(define (apply-generic op . args)
	(define (raise-type arg1 arg2)
		(let
			((supertype (get 'raise (list (type-tag arg1)))))
			(cond
				((eq? (type-tag arg1) (type-tag arg2)) arg1)
				(supertype (raise-type (supertype (contents arg1)) arg2))
				(else #f))))
	(define (drop-types result)
		(if (or (eq? op 'raise) (eq? op 'drop)
				(and (not (pair? result)) (not (number? result)))
				(eq? (type-tag result) 'scheme-number)
				(eq? (type-tag result) 'polynomial))
			result
			(let ((drop-value (drop result)))
				(if (equal? result drop-value)
					drop-value
					(drop-types drop-value)))))

	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args))
				(if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags))))
					(let ((a1 (car args))
						  (a2 (cadr args)))
						(let ((arg1 (raise-type a1 a2))
								(arg2 (raise-type a2 a1)))
							(cond (arg1 (apply-generic op arg1 a2))
									(arg2 (apply-generic op a1 arg2))
									(else (error "No method for these types" (list op type-tags))))))
					(error "No method for these types" (list op type-tags)))))))

(define (install-scheme-number-package)
	(define (tag x) (attach-tag 'scheme-number x))
	(put 'add '(scheme-number scheme-number)
		(lambda (x y) (tag (+ x y))))
	(put 'sub '(scheme-number scheme-number)
		(lambda (x y) (tag (- x y))))
	(put 'mul '(scheme-number scheme-number)
		(lambda (x y) (tag (* x y))))
	(put 'div '(scheme-number scheme-number)
		(lambda (x y) (tag (/ x y))))
	(put 'make 'scheme-number (lambda (x) (tag x)))
	(put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
	(put '=zero? '(scheme-number) (lambda (x) (= x 0)))
	(put 'sign '(scheme-number) (lambda (x) (tag (* x -1))))
	'done)

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

(define (install-rational-package)
	;; 内部手続き
	(define (numer x) (car x))
	(define (denom x) (cdr x))
	(define (make-rat n d) (cons n d))
	(define (add-rat x y)
			(make-rat (add (mul (numer x) (denom y))
					 (mul (numer y) (denom x)))
				  (mul (denom x) (denom y))))
	(define (sub-rat x y)
		(make-rat (sub (mul (numer x) (denom y))
					 (mul (numer y) (denom x)))
				  (mul (denom x) (denom y))))
	(define (mul-rat x y)
		(make-rat (mul (numer x) (numer y))
				  (mul (denom x) (denom y))))
	(define (div-rat x y)
		(make-rat (mul (numer x) (denom y))
				  (mul (denom x) (numer y))))
	(define (equ? x y)
		(and (= (numer x) (numer y)) (= (denom x) (denom y))))
	(define (scheme-number->rational scheme-number)
		(tag (make-rat scheme-number 1)))
	(define (rational->scheme-number r)
		(let ((drop-value (numer r)))
			(if (equ? r (contents (raise drop-value)))
				drop-value
				(tag r))))
	;; システムのほかの部分とのインターフェイス
	(define (tag x) (attach-tag 'rational x))
	(put 'add '(rational rational)
		(lambda (x y) (tag (add-rat x y))))
	(put 'sub '(rational rational)
		(lambda (x y) (tag (sub-rat x y)))) 
	(put 'mul '(rational rational)
		(lambda (x y) (tag (mul-rat x y)))) 
	(put 'div '(rational rational)
		(lambda (x y) (tag (div-rat x y)))) 
	(put 'make 'rational
		(lambda (n d) (tag (make-rat n d))))
	(put 'equ? '(rational rational) equ?)
	(put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
	(put 'sign '(rational) (lambda (x) (tag (make-rat (* (numer x) -1) (denom x)))))
	(put 'raise '(scheme-number) scheme-number->rational)
	(put 'drop '(rational) rational->scheme-number)
	'done)
(define (make-rational n d) ((get 'make 'rational) n d))

(define (install-real-number-package)
	(define (equ? x y) (= x y))
	(define (rational->real-number rational)
		(if (= (car rational) 0)
			(tag 0.0)
			(tag (* (/ (car rational) (cdr rational)) 1.0))))
	(define (real-number->scheme-number original)
		(let ((drop-value (round-nop original)))
			(if (equ? original (raise (raise drop-value)))
				drop-value
				original)))

	(define (tag x) (attach-tag 'real-number x))
	(put 'add '(real-number real-number)
		(lambda (x y) (tag (+ x y))))
	(put 'sub '(real-number real-number)
		(lambda (x y) (tag (- x y))))
	(put 'mul '(real-number real-number)
		(lambda (x y) (tag (* x y))))
	(put 'div '(real-number real-number)
		(lambda (x y) (tag (/ x y))))
	(put 'make 'real-number (lambda (x) (tag (* x 1.0))))
	(put 'equ? '(real-number real-number) equ?)
	(put '=zero? '(real-number) (lambda (x) (= x 0)))
	(put 'sign '(real-number) (lambda (x) (tag (* x -1))))
	(put 'raise '(rational) rational->real-number)
	(put 'drop '(real-number) real-number->scheme-number)
	'done)
(define (make-real-number x) ((get 'make 'real-number) x))

(define (install-complex-package)
	;; 直交形式パッケージと極形式パッケージからインポートした手続き
	(define (make-from-real-imag x y)
		((get 'make-from-real-imag 'rectangular) x y))
	(define (make-from-mag-ang r a)
		((get 'make-from-mag-ang 'polar) r a))
	;; 内部手続き
	(define (add-complex z1 z2)
		(make-from-real-imag (+ (real-part z1) (real-part z2))
							 (+ (imag-part z1) (imag-part z2))))
	(define (sub-complex z1 z2)
		(make-from-real-imag (- (real-part z1) (real-part z2))
							 (- (imag-part z1) (imag-part z2))))
	(define (mul-complex z1 z2)
		(make-from-mag-ang (* (magnitude z1) (magnitude z2))
							(+ (angle z1) (angle z2))))
	(define (div-complex z1 z2)
		(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
							(- (angle z1) (angle z2))))
	(define (equ? x y)
		(and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
	(define (real-number->complex real-number)
		(make-complex-from-real-imag (contents real-number) 0))
	(define (complex->real-number c)
		(let ((real (real-part c))
			  (imag (imag-part c))
			  (original (tag c)))
			(if (not (= imag 0))
				original
				(let ((drop-value (make-real-number real)))
					(if (equ? original (raise drop-value))
						drop-value
						original)))))

	;; システムのほかの部分とのインターフェイス
	(define (tag z) (attach-tag 'complex z))
	(put 'add '(complex complex)
		(lambda (z1 z2) (tag (add-complex z1 z2))))
	(put 'sub '(complex complex)
		(lambda (z1 z2) (tag (sub-complex z1 z2))))
	(put 'mul '(complex complex)
		(lambda (z1 z2) (tag (mul-complex z1 z2))))
	(put 'div '(complex complex)
		(lambda (z1 z2) (tag (div-complex z1 z2))))
	(put 'make-from-real-imag 'complex
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'complex
		(lambda (r a) (tag (make-from-mag-ang r a))))

	(put 'real-part '(complex) real-part)
	(put 'imag-part '(complex) imag-part)
	(put 'magnitude '(complex) magnitude)
	(put 'angle '(complex) angle)

	(put 'equ? '(complex complex) equ?)
	(put '=zero? '(complex) (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
	(put 'sign '(complex) (lambda (x) (tag (make-from-real-imag (sign (real-part x)) (sign (imag-part x))))))
	(put 'raise '(real-number) real-number->complex)
	(put 'drop '(complex) complex->real-number)

	'done)

(define (install-rectangular-package)
	;; 内部手続き
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (make-from-real-imag x y) (cons x y))
	(define (magnitude z)
		(sqrt (+ (square (real-part z)) (square (imag-part z)))))
	(define (angle z)
		(atan (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a)
		(cons (* r (cos a)) (* r (sin a))))
	;; システムのほかの部分とのインターフェイス
	(define (tag x) (attach-tag 'rectangular x))
	(put 'real-part '(rectangular) real-part)
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(put 'make-from-real-imag 'rectangular
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'rectangular
		(lambda (r a) (tag (make-from-mag-ang r a))))
	'done)


(define (install-polar-package)
	;; 内部手続き
	(define (magnitude z) (car z))
	(define (angle z) (cdr z))
	(define (make-from-mag-ang r a) (cons r a))
	(define (real-part z) (* (magnitude z) (cos (angle z))))
	(define (imag-part z) (* (magnitude z) (sin (angle z))))
	(define (make-from-real-imag x y)
		(cons (sqrt (+ (square x) (square y))) (atan y x)))
	;; システムのほかの部分とのインターフェイス
	(define (tag x) (attach-tag 'polar x))
	(put 'real-part '(polar) real-part)
	(put 'imag-part '(polar) imag-part)
	(put 'magnitude '(polar) magnitude)
	(put 'angle '(polar) angle)
	(put 'make-from-real-imag 'polar
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'polar
		(lambda (r a) (tag (make-from-mag-ang r a))))
	'done)

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (raise v) (apply-generic 'raise v))
(define (drop v) (apply-generic 'drop v))
(define (sign v) (apply-generic 'sign v))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (add v1 v2) (apply-generic 'add v1 v2))
(define (mul v1 v2) (apply-generic 'mul v1 v2))
(define (sub v1 v2) (apply-generic 'sub v1 v2))
(define (div v1 v2) (apply-generic 'div v1 v2))

(install-scheme-number-package)
(install-rational-package)
(install-real-number-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)

; #####

; start

(define (install-polynomial-package)
	;; 内部手続き
	;; poly の表現
	(define (make-poly variable term-list) (cons variable term-list))
	(define (variable p) (car p))
	(define (term-list p) (cdr p))

	(define (variable? x) (symbol? x))
	(define (same-variable? v1 v2)
		(and (variable? v1) (variable? v2) (eq? v1 v2)))
	;; 項と項リストの表現
	(define (adjoin-term term term-list)
		(if (=zero? (coeff term))
			term-list
			(cons term term-list)))
	(define (the-empty-termlist) '())
	(define (first-term term-list) (car term-list))
	(define (rest-terms term-list) (cdr term-list))
	(define (empty-termlist? term-list) (null? term-list))
	(define (make-term order coeff) (list order coeff))
	(define (order term) (car term))
	(define (coeff term) (cadr term))

	(define (add-terms L1 L2)
		(cond ((empty-termlist? L1) L2)
			  ((empty-termlist? L2) L1)
			  (else
				(let ((t1 (first-term L1))
					(t2 (first-term L2)))
					(cond
						((> (order t1) (order t2))
							(adjoin-term t1 (add-terms (rest-terms L1) L2)))
						((< (order t1) (order t2))
							(adjoin-term t2 (add-terms L1 (rest-terms L2))))
						(else (adjoin-term
								(make-term
										(order t1)
										(add (coeff t1) (coeff t2)))
								(add-terms
									(rest-terms L1)
									(rest-terms L2)))))))))

	(define (sub-terms L1 L2) (add-terms L1 (sign-term L2)))

	(define (add-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
			(make-poly (variable p1)
				(add-terms (term-list p1) (term-list p2)))
			(error "Polys not in same var: ADD-POLY" (list p1 p2))))
	
	(define (mul-terms L1 L2)
		(if (empty-termlist? L1)
			(the-empty-termlist)
			(add-terms 
				(mul-term-by-all-terms (first-term L1) L2)
				(mul-terms (rest-terms L1) L2))))
	(define (mul-term-by-all-terms t1 L)
		(if (empty-termlist? L)
			(the-empty-termlist)
			(let ((t2 (first-term L)))
				(adjoin-term
					(make-term
						(+ (order t1) (order t2))
						(mul (coeff t1) (coeff t2)))
					(mul-term-by-all-terms t1 (rest-terms L))))))

	(define (mul-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
			(make-poly (variable p1)
				(mul-terms (term-list p1) (term-list p2)))
			(error "Polys not in same var: MUL-POLY" (list p1 p2))))
	
	(define (div-terms L1 L2) ; L1='((5 1) (0 -1)) L2='((2 1) (0 -1))
		(if (empty-termlist? L1)
			(list (the-empty-termlist) (the-empty-termlist))
			(let ((t1 (first-term L1))(t2 (first-term L2)))
				(if (> (order t2) (order t1))
					(list (the-empty-termlist) L1)
					(let ((new-c (div (coeff t1) (coeff t2)))
						  (new-o (- (order t1) (order t2))))
						(let ((rest-of-result
							; ⟨ 再帰的に残りを計算する⟩
							(div-terms (sub-terms L1 (mul-terms (list (make-term new-o new-c)) L2)) L2)
							))
						; ⟨ 完全な結果を作る⟩
						(list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))
						))))))

	(define (div-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
			(make-poly (variable p1)
				(div-terms (term-list p1) (term-list p2)))
			(error "Polys not in same var: DIV-POLY" (list p1 p2))))

	(define (zero?-poly p)
		(zero?-terms (term-list p)))
	(define (zero?-terms L)
		(or (empty-termlist? L)
			(and (=zero? (first-term L)) (zero?-terms (rest-terms L)))))

	(define (sign-term L)
		(map (lambda (x)
				(make-term
					(order x)
					(sign (coeff x))))
			L))

	(define (sign-poly p)
		(make-poly 
			(variable p)
			(sign-term (term-list p))))


	;; システムのほかの部分とのインターフェイス
	(define (tag p) (attach-tag 'polynomial p))
	(put 'add '(polynomial polynomial)
		(lambda (p1 p2) (tag (add-poly p1 p2))))
	(put 'mul '(polynomial polynomial)
		(lambda (p1 p2) (tag (mul-poly p1 p2))))
	(put 'sub '(polynomial polynomial)
		(lambda (p1 p2) (tag (add-poly p1 (sign-poly p2)))))
	(put 'div '(polynomial polynomial)
		(lambda (p1 p2) (tag (div-poly p1 p2))))
	(put 'make 'polynomial
		(lambda (var terms) (tag (make-poly var terms))))
	(put '=zero? '(polynomial) (lambda (p) (zero?-poly p)))
	(put 'sign '(polynomial) (lambda (p) (tag (sign-poly p))))
	'done)
(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))

(install-polynomial-package)

(define sample1 (make-polynomial 'x '((2 5) (1 3) (0 7))))
(define sample2 (make-polynomial 'x '((2 2))))
(define sample3 (make-polynomial 'x '((3 2))))
sample1

(add sample1 sample2)
(add sample1 sample3)
(mul sample1 sample2)
(mul sample1 sample3)

(newline)

(sign sample1)
(sub sample1 sample2)
(sub sample1 sample3)
(sub sample3 sample1)

(newline)

(define div1 (make-polynomial 'x '((5 1) (0 -1))))
(define div2 (make-polynomial 'x '((2 1) (0 -1))))

(display "div1 ")div1
(display "div2 ")div2

(div div1 div2)

(newline)

(define r1 (make-rational 5 3))
(define r2 (make-rational 5 1))
(add r1 r2)
(newline)

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

(display "p1 ")p1
(display "p2 ")p2
(display "rf ")rf

(add rf rf)
