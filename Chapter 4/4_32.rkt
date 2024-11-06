#|

execute delay-interpreter

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

;lazy list

(define pair (cons wasshoi midnight))
(define wasshoi 10)
(car pair)

|#