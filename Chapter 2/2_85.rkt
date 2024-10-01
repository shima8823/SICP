; 良問
#|

どうやら勘違いをしていたようだ。
まず、projectとというのをコード側にも定義する必要がある。
これは射影1つdropするというもの。
比べて書いたコードはdropを1回呼び出したら1回dropするというもの。
これはこれでもいいが問題を読んで忠実に再現するべきだった。
現在のコードの改善点は各型のパッケージに
1階層してに変換する内部手続き(equ? original drop-v)
をまとめるべきだった。

仕様は間違っていたが機能としては達成できているので合っていることとする。

|#

#lang racket

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
		(if (or (eq? op 'raise) (eq? op 'drop) (eq? (type-tag result) 'scheme-number))
			result
			(let ((drop-value (drop result)))
				(if (equal? result drop-value)
					drop-value
					(drop-types drop-value)))))

	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(drop-types (apply proc (map contents args)))
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
	'done)

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

(define (install-rational-package)
	;; 内部手続き
	(define (numer x) (car x))
	(define (denom x) (cdr x))
	(define (make-rat n d)
		(let ((g (gcd n d)))
			(cons (/ n g) (/ d g))))
	(define (add-rat x y)
		(make-rat (+ (* (numer x) (denom y))
					 (* (numer y) (denom x)))
				  (* (denom x) (denom y))))
	(define (sub-rat x y)
		(make-rat (- (* (numer x) (denom y))
					 (* (numer y) (denom x)))
				  (* (denom x) (denom y))))
	(define (mul-rat x y)
		(make-rat (* (numer x) (numer y))
				  (* (denom x) (denom y))))
	(define (div-rat x y)
		(make-rat (* (numer x) (denom y))
				  (* (denom x) (numer y))))
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
(define (equ? x y) (apply-generic 'equ? x y))
(define (add v1 v2) (apply-generic 'add v1 v2))

(install-scheme-number-package)
(install-rational-package)
(install-real-number-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)

(define same-r (make-rational 5 3))
(define drop-r (make-rational 5 1))

(define same-real (make-real-number 2.5))
(define drop-real (make-real-number 5.0))

(define same-c (make-complex-from-real-imag 2.5 10))
(define drop-c (make-complex-from-real-imag 5.5 0))

(newline)
(drop same-c)
(drop drop-c)

; (round-nop 2.5)
(drop same-real)
(drop drop-real)

(drop same-r)
(drop drop-r)

(define until-real (make-complex-from-real-imag 1.5 0))
(define until-integer (make-complex-from-real-imag 1 0))
(define until-complex (make-complex-from-real-imag 2 3))


(add until-real 0)
(add until-integer 0)
(add until-complex 0)
