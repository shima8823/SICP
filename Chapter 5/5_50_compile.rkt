#lang sicp

(#%require "util_metacircular.rkt")

(define (compile exp target linkage)
	(cond
		((self-evaluating? exp)
			(compile-self-evaluating exp target linkage))
		((quoted? exp)
			(compile-quoted exp target linkage))
		((variable? exp)
			(compile-variable exp target linkage))
		((assignment? exp)
			(compile-assignment exp target linkage))
		((definition? exp)
			(compile-definition exp target linkage))
		((if? exp)
			(compile-if exp target linkage))
		((lambda? exp)
			(compile-lambda exp target linkage))
		((begin? exp)
			(compile-sequence
				(begin-actions exp) target linkage))
		((cond? exp)
			(compile (cond->if exp) target linkage))
		((application? exp)
			(compile-application exp target linkage))
		(else
			(error " Unknown expression type : COMPILE " exp))))

(define (make-instruction-sequence needs modifies statements)
	(list needs modifies statements))

(define (empty-instruction-sequence)
	(make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
	(cond
		((eq? linkage 'return)
			(make-instruction-sequence '(continue) '()
				'((goto (reg continue)))))
		((eq? linkage 'next)
			(empty-instruction-sequence))
		(else
			(make-instruction-sequence '() '()
				`((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
	(preserving '(continue) instruction-sequence (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
	(end-with-linkage linkage
		(make-instruction-sequence '() (list target)
			`((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage)
	(end-with-linkage linkage
		(make-instruction-sequence '() (list target) 
			`((assign ,target (const ,(text-of-quotation exp)))))))
(define (compile-variable exp target linkage)
	(end-with-linkage linkage
		(make-instruction-sequence '(env) (list target)
			`((assign ,target (op lookup-variable-value) (const ,exp) (reg env))))))

(define (compile-assignment exp target linkage)
	(let ((var (assignment-variable exp))
		  (get-value-code
			(compile (assignment-value exp) 'val 'next)))
		(end-with-linkage linkage
			(preserving '(env)
				get-value-code
			 	(make-instruction-sequence '(env val) (list target)
					`((perform (op set-variable-value!)
								(const ,var)
								(reg val)
								(reg env))
					  (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
	(let ((var (definition-variable exp))
		  (get-value-code
			(compile (definition-value exp) 'val 'next)))
		(end-with-linkage linkage
			(preserving '(env)
				get-value-code
				(make-instruction-sequence '(env val) (list target)
					`((perform (op define-variable!)
								(const ,var)
								(reg val)
								(reg env))
					  (assign ,target (const ok))))))))


(define label-counter 0)
(define (new-label-number)
	(set! label-counter (+ 1 label-counter))
	label-counter)
(define (make-label name)
	(string->symbol
		(string-append
			(symbol->string name)
			(number->string (new-label-number)))))

(define (compile-if exp target linkage)
	(let ((t-branch (make-label 'true-branch))
		  (f-branch (make-label 'false-branch))
		  (after-if (make-label 'after-if)))
		(let ((consequent-linkage
				(if (eq? linkage 'next) after-if linkage)))
			(let ((p-code (compile (if-predicate exp) 'val 'next))
				  (c-code (compile (if-consequent exp) target consequent-linkage))
				  (a-code (compile (if-alternative exp) target linkage)))
				(preserving '(env continue) p-code
					(append-instruction-sequences
						(make-instruction-sequence '(val) '()
							`((test (op false?) (reg val))
							  (branch (label ,f-branch))))
						(parallel-instruction-sequences
							(append-instruction-sequences t-branch c-code)
							(append-instruction-sequences f-branch a-code))
						after-if))))))

(define (compile-sequence seq target linkage)
	(if (last-exp? seq)
		(compile (first-exp seq) target linkage)
		(preserving '(env continue)
			(compile (first-exp seq) target 'next)
			(compile-sequence (rest-exps seq) target linkage))))

(define (make-compiled-procedure entry env)
	(list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
	(tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-lambda exp target linkage)
	(let ((proc-entry (make-label 'entry))
		  (after-lambda (make-label 'after-lambda)))
		(let ((lambda-linkage
				(if (eq? linkage 'next) after-lambda linkage)))
			(append-instruction-sequences
				(tack-on-instruction-sequence
					(end-with-linkage lambda-linkage
						(make-instruction-sequence '(env) (list target)
							`((assign ,target
								(op make-compiled-procedure)
								(label ,proc-entry)
								(reg env)))))
					(compile-lambda-body exp proc-entry))
				after-lambda))))


(define (compile-lambda-body exp proc-entry)
	(let ((formals (lambda-parameters exp)))
		(append-instruction-sequences
			(make-instruction-sequence '(env proc argl) '(env)
				`(,proc-entry
					(assign env
						(op compiled-procedure-env)
						(reg proc))
					(assign env
						(op extend-environment)
						(const ,formals)
						(reg argl)
						(reg env))))
			(compile-sequence (lambda-body exp) 'val 'return))))

(define (compile-application exp target linkage)
	(let ((proc-code (compile (operator exp) 'proc 'next))
		  (operand-codes
			(map (lambda (operand) (compile operand 'val 'next))
				(operands exp))))
		(preserving '(env continue)
			proc-code
			(preserving '(proc continue)
				(construct-arglist operand-codes)
				(compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
	(let ((operand-codes (reverse operand-codes)))
		(if (null? operand-codes)
			(make-instruction-sequence '() '(argl)
				'((assign argl (const ()))))
			(let ((code-to-get-last-arg
					(append-instruction-sequences
						(car operand-codes)
						(make-instruction-sequence '(val) '(argl)
							'((assign argl (op list) (reg val)))))))
				(if (null? (cdr operand-codes))
					code-to-get-last-arg
					(preserving '(env)
						code-to-get-last-arg
						(code-to-get-rest-args
							(cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
	(let ((code-for-next-arg
			(preserving '(argl)
				(car operand-codes)
				(make-instruction-sequence '(val argl) '(argl)
					'((assign argl
						(op cons) (reg val) (reg argl)))))))
		(if (null? (cdr operand-codes))
			code-for-next-arg
			(preserving '(env)
				code-for-next-arg
				(code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
	(let ((primitive-branch (make-label 'primitive-branch))
		  (compiled-branch (make-label 'compiled-branch))
		  (after-call (make-label 'after-call)))
		(let ((compiled-linkage
				(if (eq? linkage 'next) after-call linkage)))
			(append-instruction-sequences
				(make-instruction-sequence '(proc) '()
					`((test (op primitive-procedure?) (reg proc))
					  (branch (label ,primitive-branch))))
				(parallel-instruction-sequences
					(append-instruction-sequences
						compiled-branch
						(compile-proc-appl target compiled-linkage))
					(append-instruction-sequences
						primitive-branch
						(end-with-linkage linkage
							(make-instruction-sequence '(proc argl) (list target)
								`((assign ,target
									(op apply-primitive-procedure)
									(reg proc)
									(reg argl)))))))
				after-call))))

(define all-regs '(env proc val argl continue))

(define (compile-proc-appl target linkage)
	(cond
		((and (eq? target 'val) (not (eq? linkage 'return)))
			(make-instruction-sequence '(proc) all-regs
				`((assign continue (label ,linkage))
				  (assign val (op compiled-procedure-entry)
					(reg proc))
				  (goto (reg val)))))
		((and (not (eq? target 'val))
			  (not (eq? linkage 'return)))
			(let ((proc-return (make-label 'proc-return)))
				(make-instruction-sequence '(proc) all-regs
					`((assign continue (label ,proc-return))
					  (assign val (op compiled-procedure-entry)
						(reg proc))
					  (goto (reg val))
					  ,proc-return
					  (assign ,target (reg val))
					  (goto (label ,linkage))))))
		((and (eq? target 'val) (eq? linkage 'return))
			(make-instruction-sequence
				'(proc continue)
				all-regs
				'((assign val (op compiled-procedure-entry)
					(reg proc))
				  (goto (reg val)))))
	((and (not (eq? target 'val))
		  (eq? linkage 'return))
		(error "return linkage,target not val: COMPILE"
			target))))

(define (registers-needed s)
	(if (symbol? s) '() (car s)))
(define (registers-modified s)
	(if (symbol? s) '() (cadr s)))
(define (statements s)
	(if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
	(memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
	(memq reg (registers-modified seq)))


(define (append-instruction-sequences . seqs)
	(define (append-2-sequences seq1 seq2)
		(make-instruction-sequence
			(list-union
				(registers-needed seq1)
				(list-difference (registers-needed seq2)
								(registers-modified seq1)))
			(list-union (registers-modified seq1)
						(registers-modified seq2))
			(append (statements seq1) (statements seq2))))
	(define (append-seq-list seqs)
		(if (null? seqs)
			(empty-instruction-sequence)
			(append-2-sequences
				(car seqs)
				(append-seq-list (cdr seqs)))))
	(append-seq-list seqs))

(define (list-union s1 s2)
	(cond ((null? s1) s2)
		  ((memq (car s1) s2) (list-union (cdr s1) s2))
		  (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
	(cond ((null? s1) '())
		  ((memq (car s1) s2) (list-difference (cdr s1) s2))
		  (else (cons (car s1)
			(list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
	(if (null? regs)
		(append-instruction-sequences seq1 seq2)
		(let ((first-reg (car regs)))
			(if (and (needs-register? seq2 first-reg)
					 (modifies-register? seq1 first-reg))
				(preserving (cdr regs)
					(make-instruction-sequence
						(list-union (list first-reg)
									(registers-needed seq1))
						(list-difference (registers-modified seq1)
										 (list first-reg))
						(append `((save ,first-reg))
								(statements seq1)
								`((restore ,first-reg))))
					seq2)
				(preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
	(make-instruction-sequence
		(registers-needed seq)
		(registers-modified seq)
		(append (statements seq)
				(statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
	(make-instruction-sequence
		(list-union (registers-needed seq1)
					(registers-needed seq2))
		(list-union (registers-modified seq1)
					(registers-modified seq2))
		(append (statements seq1)
				(statements seq2))))

(display-insts
; (compile-and-go
(compile
	'(begin
; metacircular
(define (eval exp env) ((analyze exp) env))
(define (analyze exp)
	(cond
		((self-evaluating? exp) (analyze-self-evaluating exp))
		((quoted? exp) (analyze-quoted exp))
		((variable? exp) (analyze-variable exp))
		((assignment? exp) (analyze-assignment exp))
		((definition? exp) (analyze-definition exp))
		((if? exp) (analyze-if exp))
		((lambda? exp) (analyze-lambda exp))
		((begin? exp) (analyze-sequence (begin-actions exp)))
		((cond? exp) (analyze (cond->if exp)))
		((application? exp) (analyze-application exp))
		(else (error " Unknown expression type : ANALYZE " exp))))
(define (analyze-self-evaluating exp) (lambda (env) exp))
(define (analyze-quoted exp)
	(let ((qval (text-of-quotation exp)))
		(lambda (env) qval)))
(define (analyze-variable exp)
	(lambda (env) (lookup-variable-value exp env)))
(define (analyze-assignment exp)
	(let ((var (assignment-variable exp))
		  (vproc (analyze (assignment-value exp))))
		(lambda (env)
			(set-variable-value! var (vproc env) env)
		'ok)))
(define (analyze-definition exp)
	(let ((var (definition-variable exp))
		  (vproc (analyze (definition-value exp))))
		(lambda (env)
			(define-variable! var (vproc env) env)
		'ok)))
(define (analyze-if exp)
	(let ((pproc (analyze (if-predicate exp)))
		  (cproc (analyze (if-consequent exp)))
		  (aproc (analyze (if-alternative exp))))
		(lambda (env)
			(if (true? (pproc env))
				(cproc env)
				(aproc env)))))
(define (analyze-lambda exp)
	(let ((vars (lambda-parameters exp))
		  (bproc (analyze-sequence (lambda-body exp))))
		(lambda (env) (make-procedure vars bproc env))))
(define (analyze-sequence exps)
	(define (sequentially proc1 proc2)
		(lambda (env) (proc1 env) (proc2 env)))
	(define (loop first-proc rest-procs)
		(if (null? rest-procs)
			first-proc
			(loop
				(sequentially first-proc (car rest-procs))
				(cdr rest-procs))))
	(let ((procs (map analyze exps)))
		(if (null? procs)
			(error " Empty sequence : ANALYZE "))
			(loop (car procs) (cdr procs))))
(define (analyze-application exp)
	(let ((fproc (analyze (operator exp)))
		  (aprocs (map analyze (operands exp))))
		(lambda (env)
			(execute-application
				(fproc env)
				(map
					(lambda (aproc) (aproc env))
					aprocs)))))
(define (execute-application proc args)
	(cond
		((primitive-procedure? proc)
			(apply-primitive-procedure proc args))
		((compound-procedure? proc)
			((procedure-body proc)
				(extend-environment
					(procedure-parameters proc)
					args
					(procedure-environment proc))))
		(else
			(error " Unknown procedure type : EXECUTE-APPLICATION " proc))))
(define (self-apply procedure arguments)
	(cond
		((primitive-procedure? procedure)
			(apply-primitive-procedure procedure arguments))
		((compound-procedure? procedure)
			(eval-sequence
				(procedure-body procedure)
				(extend-environment
					(procedure-parameters procedure)
					arguments
					(procedure-environment procedure))))
		(else
			(error "Unknown procedure type : APPLY " procedure))))
(define (list-of-values exps env)
	(if (no-operands? exps)
		'()
		(cons (eval (first-operand exps) env)
			  (list-of-values (rest-operands exps) env))))
(define (eval-if exp env)
	(if (true? (eval (if-predicate exp) env))
		(eval (if-consequent exp) env)
		(eval (if-alternative exp) env)))
(define (eval-sequence exps env)
	(cond
		((last-exp? exps)
			(eval (first-exp exps) env))
		(else
			(eval (first-exp exps) env)
			(eval-sequence (rest-exps exps) env))))
(define (eval-assignment exp env)
	(set-variable-value!
		(assignment-variable exp)
		(eval (assignment-value exp) env)
		env)
		'ok)
(define (eval-definition exp env)
	(define-variable!
		(definition-variable exp)
		(eval (definition-value exp) env)
		env)
	'ok)
(define (self-evaluating? exp)
	(cond
		((number? exp) true)
		((string? exp) true)
		(else false)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))
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
				((eq? var (car vars)) (car vals))
				(else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error " Unbound variable " var)
			(let ((frame (first-frame env)))
				(scan
					(frame-variables frame)
					(frame-values frame)))))
	(env-loop env))
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
	(list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)
	

	; ⟨more primitives⟩
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
(define input-prompt "~~~ M-Eval input :")
(define output-prompt "~~~ M-Eval value :")
(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		(let ((output (eval input the-global-environment)))
			(announce-output output-prompt)
			(user-print output)))
	(driver-loop))
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

(driver-loop)
)
'val ; compile
'next ; compile
)
) ; display-insts
