#lang sicp

(define (install-eval-package)
	(define (quote exp env)
		(text-of-quotation exp))
	(define (assignment exp env)
		(eval-assignment exp env))
	(define (definition exp env)
		(eval-definition exp env))
	(define (if exp env)
		(eval-if exp env))
	(define (lambda exp env)
		(make-procedure
			(lambda-parameters exp)
			(lambda-body exp)
			env))
	(define (begin exp env)
		(eval-sequence (begin-actions exp) env))
	(define (cond exp env)
		(eval (cond->if exp) env))

	(put 'eval 'quote quote)
	(put 'eval 'set! assignment)
	(put 'eval 'define definition)
	(put 'eval 'if if)
	(put 'eval 'lambda lambda)
	(put 'eval 'begin begin)
	(put 'eval 'cond cond)
	'done
)

(define (eval exp env)
	(cond
		((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((application? exp)
			(apply
				(eval (operator exp) env)
				(list-of-values (operands exp) env)))
		((not (pair? exp)) (error " Unknown expression type : EVAL " exp))
		(else ((get 'eval (operator exp)) exp env)))) 
		; getでなかった場合も考えた方がいい。

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
