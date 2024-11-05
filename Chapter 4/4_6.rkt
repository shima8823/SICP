#lang sicp

(define (eval exp env)
	(cond
		((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
			(make-procedure
				(lambda-parameters exp)
				(lambda-body exp)
				env))
		((let? exp) (eval (let->combination exp) env))
		((begin? exp) (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((application? exp)
			(apply
				(eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
			(error " Unknown expression type : EVAL " exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-define-pairs exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
	(cons ; mapでリストがreturnされる ((exp) (exp) (exp))。よってconsで前にlambdaを追加する
		(make-lambda
			(map car (let-define-pairs exp))
			(let-body exp))
		(map cadr (let-define-pairs exp))))

; (let->combination '(let ((x 10) (y 11)) (* x y)))
; (let->combination '(let ((x 10) (y 11)) (define (mul x y) (* x y)) (mul x y)))