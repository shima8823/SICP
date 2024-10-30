; return

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
		((let? exp) (eval (let-combination exp) env))
		((let*? exp) (eval (let*->nested-lets exp) env)) ; これで足りるはず
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
(define (let-body exp) (caddr exp))

(define (let->combination exp)
	(list ; consでもいいかも
		(make-lambda
			(map car (let-define-pairs exp))
			(let-body exp))
		(map cadr (let-define-pairs exp))))

#|

(let* ((⟨var1⟩ ⟨exp1⟩) (⟨var2⟩ ⟨exp2⟩)... (⟨varn⟩ ⟨expn⟩))
	⟨body⟩)

(let ((var1 exp1))
	(let ((var2 exp2))
		...
		(let ((varn expn))
			⟨body⟩)))

let*はletの入れ子になる
|#

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let-define-pairs exp) (cadr exp))
(define (let-body exp) (caddr exp))

(define (expand-let define-pairs body)
	(if (null? define-pairs)
		body
		(list 'let (list (car define-pairs)) (expand-let (cdr define-pairs) body))))

(define (let*->nested-lets exp)
	(expand-let (let-define-pairs exp) (let-body exp)))

(let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5)) )(* x z)))

#|

(let ((x 3))
	(let ((y (+ x 2)))
		(let ((z (+ x y 5)))
			(* x z))))

|#