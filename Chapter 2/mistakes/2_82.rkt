; 良問
; 考え方はあっていたが最後まで書く気力がなかった

#lang racket

(define (square x) (* x x))

(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (put-coercion type1 type2 function)
  (put type1 type2 function))
(define (get-coercion type1 type2)
  (get type1 type2))

(define (attach-tag type-tag contents) 
	(if (eq? type-tag 'scheme-number)
		contents
		(cons type-tag contents)))
(define (type-tag datum)
	(cond ((pair? datum) (car datum))
		  ((number? datum) 'scheme-number)
		  (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
	(cond ((pair? datum) (cdr datum))
		  ((number? datum) datum)
		  (else (error "Bad tagged datum: CONTENTS" datum))))

(define (apply-generic op . args)
	; ; (define (identity x) x)
	; ; listでやりたい
	; ; 無理だったらfalse
	; ; 行けるんだったら変換した集合を取る
	; (define (convert type arg type-tags)
	; 	(cond
	; 		((null? type-tags) '())
	; 		((eq? type (car type-tags)) type)
	; 		(else 
	; 			(let ((type2 (car type-tags))
	; 				  (a2 (car args)))
	; 				(let ((t1->t2 (get-coercion type type2))
	; 					(t2->t1 (get-coercion type type1)))
	; 					(cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
	; 						(t2->t1 (apply-generic op a1 (t2->t1 a2)))
	; 						(else (error "No method for these types" (list op type-tags))))))
	; 		)
	; 	)
		
	; )

	; (define (iter type-tags)
	; 	(cond
	; 		((null? type-tags) (error "No method iter"))
	; 		(else 
	; 			(let (set (convert (car type-tags) ? (cdr type-tags)))
	; 				 (if set
	; 				 	set
	; 					(iter (cdr type-tags))
	; 				 )))
	; 	)
	; )

	; (let ((type-tags (map type-tag args)))
	; 	(let ((proc (get op type-tags)))
	; 		(if proc
	; 			(apply proc (map contents args))
	; 			; c
	; 			(iter )))))
	; 			; (if (= (length args) 2)
	; 			; 	(let ((type1 (car type-tags))
	; 			; 		  (type2 (cadr type-tags))
	; 			; 		  (a1 (car args))
	; 			; 		  (a2 (cadr args)))
	; 			; 		(let ((t1->t2 (get-coercion type1 type2))
	; 			; 			(t2->t1 (get-coercion type2 type1)))
	; 			; 			(cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
	; 			; 				  (t2->t1 (apply-generic op a1 (t2->t1 a2)))
	; 			; 				(else (error "No method for these types" (list op type-tags))))))
	; 			; 	(error "No method for these types" (list op type-tags)))))))
	 (define (type-tags args) 
         (map type-tag args)) 
  
     (define (try-coerce-to target) 
         (map (lambda (x) 
                 (let ((coercor (get-coercion (type-tag x) (type-tag target)))) 
                     (if coercor 
                         (coercor x) 
                         x))) 
              args)) 
      
     (define (iterate next) 
         (if (null? next)  
             (error "No coersion strategy for these types " (list op (type-tags args))) 
             (let ((coerced (try-coerce-to (car next)))) 
                 (let ((proc (get op (type-tags coerced)))) 
                     (if proc 
                         (apply proc (map contents coerced)) 
                         (iterate (cdr next))))))) 
  
     (let ((proc (get op (type-tags args)))) 
         (if proc 
             (apply proc (map contents args)) 
             (iterate args)))) 

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
	'done)
(define (make-rational n d) ((get 'make 'rational) n d))

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
	(define (scheme-number->complex n)
		(make-complex-from-real-imag (contents n) 0))

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
	(put-coercion 'scheme-number 'complex scheme-number->complex)
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

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)

(define sample-s (make-scheme-number 5))
(define sample-c (make-complex-from-real-imag 5 10))

sample-s
sample-c

(apply-generic 'add sample-s sample-c)
(apply-generic 'add sample-c sample-s)