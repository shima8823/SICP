#lang sicp

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
	(map (lambda (var val) (cons var val)) variables values))
	; (lambda ...) -> consにするべき
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
	(set-cdr! frame frame)
	(set-car! frame (cons var val)))

(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
		(cons (make-frame vars vals) base-env)
		(if (< (length vars) (length vals))
			(error "Too many arguments supplied " vars vals)
			(error "Too few arguments supplied " vars vals))))

(define (lookup-variable-value var env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond
				((null? vars)
					(env-loop (enclosing-environment env)))
				((eq? var (car vars)) (car vals))
				(else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error " Unbound variable " var)
			(let ((frame (first-frame env)))
				(scan
					(frame-variables frame)
					(frame-values frame)))))
	(env-loop env))

; scanした後setを更新する必要あるかも
(define (set-variable-value! var val env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond
				((null? vars)
					(env-loop (enclosing-environment env)))
				((eq? var (car vars)) (set-car! vals val))
				(else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error " Unbound variable : SET! " var)
			(let ((frame (first-frame env)))
				(scan
					(frame-variables frame)
					(frame-values frame)))))
	(env-loop env))

; scanした後setを更新する必要あるかも
(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(define (scan vars vals)
			(cond
				((null? vars)
					(add-binding-to-frame! var val frame))
				((eq? var (car vars)) (set-car! vals val))
				(else (scan (cdr vars) (cdr vals)))))
		(scan (frame-variables frame) (frame-values frame))))