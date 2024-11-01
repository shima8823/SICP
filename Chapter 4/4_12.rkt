#lang sicp
; lambda (vals), lambda (env)だけで良さそう
(define (scan fount-p nfound-p vars vals var frame env)
	(cond
		((null? vars)
			(nfound-p vars vals var frame env))
		((eq? var (car vars)) (fount-p vars vals var frame env))
		(else (scan (cdr vars) (cdr vals)))))

(define (lookup-variable-value var env)
	(define (env-loop env)
		(if (eq? env the-empty-environment)
			(error " Unbound variable " var)
			(let ((frame (first-frame env)))
				(scan
					(lambda (vars vals var frame env)
						(car vals))
					(lambda (vars vals var frame env)
						(env-loop (enclosing-environment env)))
					(frame-variables frame)
					(frame-values frame)
					var
					frame
					env))))
	(env-loop env))

(define (set-variable-value! var val env)
	(define (env-loop env)
		(if (eq? env the-empty-environment)
			(error " Unbound variable : SET! " var)
			(let ((frame (first-frame env)))
				(scan
					(lambda (vars vals var frame env)
						(set-car! vals val))
					(lambda (vars vals var frame env)
						(env-loop (enclosing-environment env)))
					(frame-variables frame)
					(frame-values frame)
					var
					frame
					env))))
	(env-loop env))

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(scan
			(lambda (vars vals var frame env)
				(set-car! vals val))
			(lambda (vars vals var frame env)
				(add-binding-to-frame! var val frame))
			(frame-variables frame)
			(frame-values frame)
			var
			frame
			env)))