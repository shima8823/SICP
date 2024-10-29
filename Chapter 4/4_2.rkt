#lang sicp

(define (louis-eval exp env)
	(cond
		((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((application? exp)
			(apply
				(eval (operator exp) env)
				(list-of-values (operands exp) env)))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
			(make-procedure
				(lambda-parameters exp)
				(lambda-body exp)
				env))
		((begin? exp) (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		(else
			(error " Unknown expression type : EVAL " exp))))

; (define (application? exp) (pair? exp))
(define (application? exp) (tagged-list? exp 'call))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (definition? exp) (tagged-list? exp 'define))

; (define (operator exp) (car exp))
(define (operator exp) (cadr exp))

; (define (operands exp) (cdr exp))
(define (operands exp) (cddr exp))

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))

#|

a.
(define x 3) 
この式はpairと認識されて手続き適用の節とインタプリタは判断する。
文字通りdefine手続きをx, 3に適用する。

b.
Line 25

|#
