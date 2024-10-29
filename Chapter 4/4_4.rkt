#lang sicp

(define (eval exp env)
	(cond
		((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))

		; add
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))

		((lambda? exp)
			(make-procedure
				(lambda-parameters exp)
				(lambda-body exp)
				env))
		((begin? exp) (eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((application? exp)
			(apply
				(eval (operator exp) env)
				(list-of-values (operands exp) env)))
		(else
			(error " Unknown expression type : EVAL " exp))))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

; オリジナル版はcondを使ってややこしいので書いてない。
; そもそも派生版を思いついてしまった。
; 便利なのでこれでよい

; (and (= 3 3) (= 10 5))
; (and (eval) (eval))
(define (eval-and exp env)
	(if (last-exp? exp)
		'true
		(if (true? (eval (cadr exp) env))
			(eval-and (cdr exp) env)
			'false)))

; (or (= 1 3) (= 5 5))
; (or (eval) (eval))
(define (eval-or exp env)
	(if (last-exp? exp)
		'false
		(if (true? (eval (cadr exp) env))
			'true
			(eval-or (cdr exp) env))))

; (and) ;t
; (or)  ;f

; 派生版
(define (eval-and exp env)
	(if (last-exp? exp)
		'true
		(if (true? (eval (cadr exp) env))
			(eval-and (cdr exp) env)
			'false)))

(define (eval-or exp env)
	(if (last-exp? exp)
		'false
		(if (true? (eval (cadr exp) env))
			'true
			(eval-or (cdr exp) env))))