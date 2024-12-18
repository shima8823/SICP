#lang sicp

(define (make-machine register-names ops controller-text)
	(let ((machine (make-new-machine)))
		(for-each
			(lambda (register-name)
				((machine 'allocate-register) register-name))
			register-names)
		((machine 'install-operations) ops)
		((machine 'install-instruction-sequence)
		 (assemble controller-text machine))
		machine))

(define (make-register name)
	(let ((contents '*unassigned*))
		(define (dispatch message)
			(cond ((eq? message 'get) contents)
				  ((eq? message 'set)
					(lambda (value) (set! contents value)))
				  (else
					(error " Unknown request : REGISTER " message))))
		dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))

(define (make-stack)
	(let ((s '())
		  (number-pushes 0)
		  (max-depth 0)
		  (current-depth 0))
		(define (push x)
			(set! s (cons x s))
			(set! number-pushes (+ 1 number-pushes))
			(set! current-depth (+ 1 current-depth))
			(set! max-depth (max current-depth max-depth)))
		(define (pop)
			(if (null? s)
				(error " Empty stack : POP")
				(let ((top (car s)))
					(set! s (cdr s))
					(set! current-depth (- current-depth 1))
					top)))
		(define (initialize)
			(set! s '())
			(set! number-pushes 0)
			(set! max-depth 0)
			(set! current-depth 0)
			'done)
		(define (print-statistics)
			(newline)
			(display (list 'total-pushes '= number-pushes
						   'maximum-depth '= max-depth)))
		(define (dispatch message)
			(cond ((eq? message 'push) push)
				  ((eq? message 'pop) (pop))
				  ((eq? message 'initialize) (initialize))
				  ((eq? message 'print-statistics)
					(print-statistics))
				  (else (error " Unknown request : STACK " message))))
		dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
	(let ((pc (make-register 'pc))
		  (flag (make-register 'flag))
		  (stack (make-stack))
		  (the-instruction-sequence '()))
		(let ((the-ops
				(list (list 'initialize-stack
						(lambda () (stack 'initialize)))
					  (list 'print-stack-statistics
						(lambda () (stack 'print-statistics)))))
			  (register-table (list (list 'pc pc) (list 'flag flag))))
			(define (allocate-register name)
				(if (assoc name register-table)
					(error " Multiply defined register : " name)
					(set! register-table
						(cons (list name (make-register name)) register-table)))
				'register-allocated)
			(define (lookup-register name)
				(let ((val (assoc name register-table)))
					(if val
						(cadr val)
						(error " Unknown register :" name))))
			(define (execute)
				(let ((insts (get-contents pc)))
					(if (null? insts)
						'done
						(begin
							((instruction-execution-proc (car insts)))
							(execute)))))
			(define (dispatch message)
				(cond ((eq? message 'start)
						(set-contents! pc the-instruction-sequence)
						(execute))
					  ((eq? message 'install-instruction-sequence)
						(lambda (seq)
							(set! the-instruction-sequence seq)))
					  ((eq? message 'allocate-register) allocate-register)
					  ((eq? message 'get-register) lookup-register)
					  ((eq? message 'install-operations)
						(lambda (ops)
							(set! the-ops (append the-ops ops))))
					  ((eq? message 'stack) stack)
					  ((eq? message 'operations) the-ops)
					  (else (error " Unknown request : MACHINE " message))))
			dispatch)))


(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
	(get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
	(set-contents! (get-register machine register-name) value)
	'done)

(define (get-register machine reg-name)
	((machine 'get-register) reg-name))

(define (assemble controller-text machine)
	(extract-labels
		controller-text
		(lambda (insts labels)
			(update-insts! insts labels machine)
			insts)))

(define (extract-labels text receive)
	(if (null? text)
		(receive '() '())
		(extract-labels
			(cdr text)
			(lambda (insts labels)
				(let ((next-inst (car text)))
					(if (symbol? next-inst)
						(receive
							insts
							(cons (make-label-entry next-inst insts) labels))
						(receive
							(cons (make-instruction next-inst) insts)
							labels)))))))

(define (update-insts! insts labels machine)
	(let ((pc (get-register machine 'pc))
		  (flag (get-register machine 'flag))
		  (stack (machine 'stack))
		  (ops (machine 'operations)))
		(for-each
			(lambda (inst)
				(set-instruction-execution-proc!
					inst
					(make-execution-procedure
					  (instruction-text inst)
					  labels machine pc flag stack ops)))
			insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
	(set-cdr! inst proc))
(define (make-label-entry label-name insts)
	(cons label-name insts))
(define (lookup-label labels label-name)
	(let ((val (assoc label-name labels)))
		(if val
			(cdr val)
			(error " Undefined label : ASSEMBLE "
			label-name))))

; 5.2.3

(define (make-execution-procedure inst labels machine pc flag stack ops)
	(cond ((eq? (car inst) 'assign)
			(make-assign inst machine labels ops pc))
		  ((eq? (car inst) 'test)
			(make-test inst machine labels ops flag pc))
		  ((eq? (car inst) 'branch)
			(make-branch inst machine labels flag pc))
		  ((eq? (car inst) 'goto)
			(make-goto inst machine labels pc))
		  ((eq? (car inst) 'save)
			(make-save inst machine stack pc))
		  ((eq? (car inst) 'restore)
			(make-restore inst machine stack pc))
		  ((eq? (car inst) 'perform)
			(make-perform inst machine labels ops pc))
		  (else
			(error "Unknown instruction type : ASSEMBLE " inst))))

(define (make-assign inst machine labels operations pc)
	(let ((target (get-register machine (assign-reg-name inst)))
		  (value-exp (assign-value-exp inst)))
		(let ((value-proc
			(if (operation-exp? value-exp)
				(make-operation-exp
					value-exp machine labels operations)
				(make-primitive-exp
					(car value-exp) machine labels))))
			(lambda () ; assign に対する実⾏⼿続き
				(set-contents! target (value-proc))
				(advance-pc pc)))))

(define (assign-reg-name assign-instruction)
	(cadr assign-instruction))
(define (assign-value-exp assign-instruction)
	(cddr assign-instruction))

(define (advance-pc pc)
	(set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
	(let ((condition (test-condition inst)))
		(if (operation-exp? condition)
			(let ((condition-proc
				(make-operation-exp
					condition machine labels operations)))
				(lambda ()
					(set-contents! flag (condition-proc))
					(advance-pc pc)))
			(error "Bad TEST instruction : ASSEMBLE " inst))))
(define (test-condition test-instruction)
	(cdr test-instruction))

(define (make-branch inst machine labels flag pc)
	(let ((dest (branch-dest inst)))
		(if (label-exp? dest)
			(let ((insts
				(lookup-label
					labels
					(label-exp-label dest))))
				(lambda ()
					(if (get-contents flag)
						(set-contents! pc insts)
						(advance-pc pc))))
			(error "Bad BRANCH instruction : ASSEMBLE " inst))))
(define (branch-dest branch-instruction)
	(cadr branch-instruction))

(define (make-goto inst machine labels pc)
	(let ((dest (goto-dest inst)))
		(cond
			((label-exp? dest)
				(let ((insts (lookup-label
					labels
					(label-exp-label dest))))
					(lambda () (set-contents! pc insts))))
			((register-exp? dest)
				(let ((reg (get-register
					machine
					(register-exp-reg dest))))
					(lambda ()
						(set-contents! pc (get-contents reg)))))
			(else (error "Bad GOTO instruction : ASSEMBLE " inst)))))
(define (goto-dest goto-instruction)
	(cadr goto-instruction))

(define (make-save inst machine stack pc)
	(let ((reg (get-register machine
		(stack-inst-reg-name inst))))
		(lambda ()
			(push stack (get-contents reg))
			(advance-pc pc))))
(define (make-restore inst machine stack pc)
	(let ((reg (get-register machine
		(stack-inst-reg-name inst))))
		(lambda ()
			(set-contents! reg (pop stack))
			(advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
	(cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
	(let ((action (perform-action inst)))
		(if (operation-exp? action)
			(let ((action-proc
				(make-operation-exp
					action machine labels operations)))
				(lambda () (action-proc) (advance-pc pc)))
			(error "Bad PERFORM instruction : ASSEMBLE " inst))))
(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
	(cond
		((constant-exp? exp)
			(let ((c (constant-exp-value exp)))
				(lambda () c)))
		((label-exp? exp)
			(let ((insts (lookup-label labels
					(label-exp-label exp))))
			(lambda () insts)))
		((register-exp? exp)
			(let ((r (get-register machine (register-exp-reg exp))))
				(lambda () (get-contents r))))
		(else (error " Unknown expression type : ASSEMBLE " exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
	(let ((op (lookup-prim (operation-exp-op exp) operations))
		  (aprocs
			(map (lambda (e)
				(make-primitive-exp e machine labels))
			(operation-exp-operands exp))))
		(lambda ()
			(apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
(and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
(cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
(cdr operation-exp))

(define (lookup-prim symbol operations)
	(let ((val (assoc symbol operations)))
		(if val
			(cadr val)
			(error " Unknown operation : ASSEMBLE " symbol))))

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))

(define evaluator-machine)
	(make-machine
		'(exp env val continue proc argl unev)
		(list
		  (list ))
		'(
eval-dispatch
	(test (op self-evaluating?) (reg exp))
	(branch (label ev-self-eval))
	(test (op variable?) (reg exp))
	(branch (label ev-variable))
	(test (op quoted?) (reg exp))
	(branch (label ev-quoted))
	(test (op assignment?) (reg exp))
	(branch (label ev-assignment))
	(test (op definition?) (reg exp))
	(branch (label ev-definition))
	(test (op if?) (reg exp))
	(branch (label ev-if))
	(test (op lambda?) (reg exp))
	(branch (label ev-lambda))
	(test (op begin?) (reg exp))
	(branch (label ev-begin))
	(test (op application?) (reg exp))
	(branch (label ev-application))
	(goto (label unknown-expression-type))
ev-self-eval
	(assign val (reg exp))
	(goto (reg continue))
ev-variable
	(assign val (op lookup-variable-value) (reg exp) (reg env))
	(goto (reg continue))
ev-quoted
	(assign val (op text-of-quotation) (reg exp))
	(goto (reg continue))
ev-lambda
	(assign unev (op lambda-parameters) (reg exp))
	(assign exp (op lambda-body) (reg exp))
	(assign val (op make-procedure) (reg unev) (reg exp) (reg env))
	(goto (reg continue))
ev-application
	(save continue)
	(save env)
	(assign unev (op operands) (reg exp))
	(save unev)
	(assign exp (op operator) (reg exp))
	(assign continue (label ev-appl-did-operator))
	(goto (label eval-dispatch))
ev-appl-did-operator
	(restore unev) (restore env)
	(assign argl (op empty-arglist))
	(assign proc (reg val)) (test (op no-operands?) (reg unev))
	(branch (label apply-dispatch))
	(save proc)
ev-appl-operand-loop
	(save argl)
	(assign exp (op first-operand) (reg unev))
	(test (op last-operand?) (reg unev))
	(branch (label ev-appl-last-arg))
	(save env)
	(save unev)
	(assign continue (label ev-appl-accumulate-arg))
	(goto (label eval-dispatch))
ev-appl-accumulate-arg
	(restore unev)
	(restore env)
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(assign unev (op rest-operands) (reg unev))
	(goto (label ev-appl-operand-loop))
ev-appl-last-arg
	(assign continue (label ev-appl-accum-last-arg))
	(goto (label eval-dispatch))
ev-appl-accum-last-arg
	(restore argl)
	(assign argl (op adjoin-arg) (reg val) (reg argl))
	(restore proc)
	(goto (label apply-dispatch))
apply-dispatch
	(test (op primitive-procedure?) (reg proc))
	(branch (label primitive-apply))
	(test (op compound-procedure?) (reg proc))
	(branch (label compound-apply))
	(goto (label unknown-procedure-type))
primitive-apply
	(assign val (op apply-primitive-procedure)
	(reg proc)
	(reg argl))
	(restore continue)
	(goto (reg continue))
compound-apply
	(assign unev (op procedure-parameters) (reg proc))
	(assign env (op procedure-environment) (reg proc))
	(assign env (op extend-environment)
		(reg unev) (reg argl) (reg env))
	(assign unev (op procedure-body) (reg proc))
	(goto (label ev-sequence))
ev-begin
	(assign unev (op begin-actions) (reg exp))
	(save continue)
	(goto (label ev-sequence))
ev-sequence
	(assign exp (op first-exp) (reg unev))
	(test (op last-exp?) (reg unev))
	(branch (label ev-sequence-last-exp))
	(save unev)
	(save env)
	(assign continue (label ev-sequence-continue))
	(goto (label eval-dispatch))
ev-sequence-continue
	(restore env)
	(restore unev)
	(assign unev (op rest-exps) (reg unev))
	(goto (label ev-sequence))
ev-sequence-last-exp
	(restore continue)
	(goto (label eval-dispatch))
ev-if
	(save exp) ; 後で使うために式を保存
	(save env)
	(save continue)
	(assign continue (label ev-if-decide))
	(assign exp (op if-predicate) (reg exp))
	(goto (label eval-dispatch)) ; 述語を評価
ev-if-decide
	(restore continue)
	(restore env)
	(restore exp)
	(test (op true?) (reg val))
	(branch (label ev-if-consequent))
ev-if-alternative
	(assign exp (op if-alternative) (reg exp))
	(goto (label eval-dispatch))
ev-if-consequent
	(assign exp (op if-consequent) (reg exp))
	(goto (label eval-dispatch))
ev-assignment
	(assign unev (op assignment-variable) (reg exp))
	(save unev) ; 後で使うために変数を保存
	(assign exp (op assignment-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-assignment-1))
	(goto (label eval-dispatch)) ; 代⼊値を評価
ev-assignment-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform (op set-variable-value!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))
ev-definition
	(assign unev (op definition-variable) (reg exp))
	(save unev) ; 後で使うために変数を保存
	(assign exp (op definition-value) (reg exp))
	(save env)
	(save continue)
	(assign continue (label ev-definition-1))
	(goto (label eval-dispatch)) ; 定義値を評価
ev-definition-1
	(restore continue)
	(restore env)
	(restore unev)
	(perform (op define-variable!) (reg unev) (reg val) (reg env))
	(assign val (const ok))
	(goto (reg continue))
		)
	)
