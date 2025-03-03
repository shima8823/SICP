#lang sicp

(#%require (only racket/base provide)) ; import provide
(#%require (only racket/base all-defined-out)) ; import symbol represent All Procedure
(provide (all-defined-out))	; Export


(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))

(define (self-evaluating? exp)
	(cond
		((number? exp) true)
		((string? exp) true)
		(else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
	(if (symbol? (cadr exp))
		(cadr exp)
		(caadr exp)))
(define (definition-value exp)
	(if (symbol? (cadr exp))
		(caddr exp)
		(make-lambda (cdadr exp) ; 仮引数
					 (cddr exp)))) ; 本体

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
	(if (not (null? (cdddr exp)))
		(cadddr exp)
		'false))

(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
	(cond
		((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
	(if (null? clauses)
		'false ; else 節はない 最終的に再起される
		(let ((first (car clauses))
			  (rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error " ELSE clause isn't last : COND->IF " clauses))
				(make-if
					(cond-predicate first)
					(sequence->exp (cond-actions first))
					(expand-clauses rest))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-define-pairs exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let->combination exp)
	(cons
		(make-lambda
			(map car (let-define-pairs exp))
			(let-body exp))
		(map cadr (let-define-pairs exp))))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
	(list 'procedure parameters body env))
(define (compound-procedure? p)
	(tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
	(cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
	(set-car! frame (cons var (car frame)))
	(set-cdr! frame (cons val (cdr frame))))

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
				((eq? var (car vars)) ; internal define
					(if (eq? (car vals) ''*unassigned*)
						(error "Unassigned variable " var)
						(car vals)))
				(else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error " Unbound variable " var)
			(let ((frame (first-frame env)))
				(scan
					(frame-variables frame)
					(frame-values frame)))))
	(env-loop env))

; internal define
(define (scan-out-defines exp)
	(define (get-defines exp)
		(if (definition? (car exp))
			(cons (car exp) (get-defines (cdr exp)))
			'()))
	(define (get-body exp)
		(if (definition? (car exp))
			(get-body (cdr exp))
			(car exp)))
	(if (definition? (car exp))
		(list
			(list
				'let
				(map
					(lambda (def)
						(list (definition-variable def) ''*unassigned*))
					(get-defines exp))
				(sequence->exp
					(map
						(lambda (def)
							(list
								'set!
								(definition-variable def)
								(definition-value def)))
						(get-defines exp)))
				(get-body exp)))
		exp))

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

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(define (scan vars vals)
			(cond
				((null? vars)
					(add-binding-to-frame! var val frame))
				((eq? var (car vars)) (set-car! vals val))
				(else (scan (cdr vars) (cdr vals)))))
		(scan (frame-variables frame) (frame-values frame))))

(define primitive-procedures
	(list
		(list '* *)
		(list '+ +)
		(list '- -)
		(list '/ /)
		(list '< <)
		(list '= =)
		(list '> >)
		(list 'apply apply)
		(list 'assoc assoc)
		(list 'atan atan)
		(list 'cadr cadr)
		(list 'car car)
		(list 'cdr cdr)
		(list 'caddr caddr)
        (list 'cdadr cdadr)
        (list 'cddr cddr)
        (list 'cadr cadr)
        (list 'cdddr cdddr)
        (list 'cadddr cadddr)
        (list 'caadr caadr)
        (list 'cadadr cadadr)
		(list 'cons cons)
		(list 'cos cos)
		(list 'display display)
		(list 'eq? eq?)
		(list 'error error)
;;      (list 'eval eval)
		(list 'length length)
		(list 'list list)
		(list 'log log)
		(list 'max max)
		(list 'map map)
		(list 'min min)
		(list 'newline newline)
		(list 'not not)
		(list 'null? null?)
		(list 'number? number?)
		(list 'pair? pair?)
		(list 'quotient quotient)
		(list 'random random)
		(list 'read read)
		(list 'remainder remainder)
		(list 'round round)
		(list 'runtime runtime)
		(list 'set-car! set-car!)
		(list 'set-cdr! set-cdr!)
		(list 'sin sin)
		(list 'symbol? symbol?)
		(list 'string? string?)
		(list 'vector-ref vector-ref)
		(list 'vector-set! vector-set!)
	))

(define (primitive-procedure-names)
	(map car primitive-procedures))
(define (primitive-procedure-objects)
	(map
		(lambda (proc) (list 'primitive (cadr proc)))
		primitive-procedures))

(define (setup-environment)
	(let ((initial-env
			(extend-environment
				(primitive-procedure-names)
				(primitive-procedure-objects)
				the-empty-environment)))
		(define-variable! 'true true initial-env)
		(define-variable! 'false false initial-env)
		initial-env))
(define the-global-environment (setup-environment))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
	(apply-in-underlying-scheme
		(primitive-implementation proc) args))

(define (prompt-for-input string)
	(newline) (newline) (display string) (newline))
(define (announce-output string)
	(newline) (display string) (newline))

(define (user-print object)
	(if (compound-procedure? object)
		(display (list
			'compound-procedure
			(procedure-parameters object)
			(procedure-body object)
			'<procedure-env>))
	(display object)))

(define (display-insts compiled)
	(map (lambda (inst) (display inst)(newline)) (caddr compiled)))

; #############################
; ## utils delay-interpreter ##
; #############################

(define (delay-it exp env)
	(list 'thunk exp env))
(define (thunk? obj)
	(tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (list-of-delayed-args exps env)
	(if (no-operands? exps)
		'()
		(cons (delay-it (first-operand exps)
				env)
			  (list-of-delayed-args (rest-operands exps)
				env))))

