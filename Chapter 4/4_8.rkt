; return

#lang sicp

(define (fib n)
	(let fib-iter
		((a 1)
		 (b 0)
		 (count n))
		(if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1)))))

(fib 10)

#|
(define (fib n)
-----
	(define (fib-iter a b count)
		body)
	(fib-iter 1 0 n))


|#

(define (let-define-pairs exp) (cadr exp))
(define (let-body exp) (caddr exp))

(define (let->combination exp)
	(if (pair? (let-define-pairs exp))
		'normal-let
		(cons
			(make-lambda
				(map car (let-define-pairs exp))
				(let-body exp))
			(map cadr (let-define-pairs exp)))
		; named let
		(list
			'begin
			(list
				'define
				(cons
					(let-define-pairs exp) ; function name
					(map car (let-define-pairs (cdr exp))))
				(let-body (cdr exp)))
			(cons
				(let-define-pairs exp) ; function name
				(map cadr (let-define-pairs (cdr exp)))))
	))

#|
(list 'begin ...)はsequence->expに(list (define...) (call))で逐次的に実行できる
(define (sequence->exp seq)
	(cond
		((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))
	
|#


(let->combination '(let fib-iter
		((a 1)
		 (b 0)
		 (count n))
		(if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1)))))

#|

(begin
	(define (fib-iter a b count)
		(if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1))))
	(fib-iter 1 0 n))

|#

; (begin (define (square x) (* x x)) (square 2))