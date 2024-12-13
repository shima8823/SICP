#lang sicp

(define (filter predicate sequence)
	(cond ((null? sequence) nil)
		  ((predicate (car sequence))
			(cons (car sequence)
				(filter predicate (cdr sequence))))
		  (else (filter predicate (cdr sequence)))))

(define (make-machine register-names ops controller-text)
	(let ((machine (make-new-machine)))
		(for-each
			(lambda (register-name)
				((machine 'allocate-register) register-name))
			register-names)
		((machine 'install-operations) ops)
		((machine 'save-machine-info) machine controller-text)
		((machine 'install-instruction-sequence)
		 (assemble controller-text machine))
		machine))

(define (make-register name)
	(let ((contents '*unassigned*))
		(define (dispatch message)
			(cond ((eq? message 'get) contents)
				  ((eq? message 'set)
					(lambda (value trace-on?)
						(if (and trace-on?
								 (not (eq? name 'pc))
								 (not (eq? contents '*unassigned*)))
							(begin
								(newline)
								(display "Register	: ") (display name) (newline)
								(if (eq? name 'continue)
									(begin
										(display "old contents	: ") (display (car (car contents))) (newline)
										(display "new contents	: ") (display (car (car value))) (newline))
									(begin
										(display "old contents	: ") (display contents) (newline)
										(display "new contents	: ") (display value) (newline)))))
						(set! contents value)))
				  (else
					(error " Unknown request : REGISTER " message))))
		dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value trace-on?) ((register 'set) value trace-on?))

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
		  (the-instruction-sequence '())
		  (trace-on? #f)
		  (reg-trace-on? #f)
		  (instruction-counter 0)
		  (breakpoints '())
		  (controller-text '())
		  (machine #f))
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
							; if insts is breakpoint then stop
							(if (not (eq? (caaar insts) 'breakpoint))
								(begin
									(if (not (eq? (caaar insts) 'label))
										(begin
											(set! instruction-counter (+ 1 instruction-counter))
											(if trace-on?
												(begin (newline) (display (caar insts))))))
									(execute)))))))
			(define (display-instruction-counter)
				(display "Execute instruction count: ")
				(display instruction-counter)
				(newline))
			(define (set-breakpoint label count)
				(set! breakpoints (cons (list label count) breakpoints))
				(set! the-instruction-sequence (assemble controller-text machine))
				'breakpoint-set)
			(define (cancel-breakpoint label count)
				(set! breakpoints
					(filter
						(lambda (b) (not (and (eq? (car b) label) (= (cadr b) count))))
						breakpoints))
				(set! the-instruction-sequence (assemble controller-text machine))
				'breakpoint-canceled)
			(define (dispatch message)
				(cond ((eq? message 'start)
						(set-contents! pc the-instruction-sequence reg-trace-on?)
						(execute))
					  ((eq? message 'install-instruction-sequence)
						(lambda (seq)
							(set! the-instruction-sequence seq)))
					  ((eq? message 'allocate-register) allocate-register)
					  ((eq? message 'get-register) lookup-register)
					;   命令カウンタ
					  ((eq? message 'get-instruction-counter) (display-instruction-counter))
					  ((eq? message 'reset-instruction-counter) (display-instruction-counter) (set! instruction-counter 0))
					  ((eq? message 'install-operations)
						(lambda (ops)
							(set! the-ops (append the-ops ops))))
					  ((eq? message 'stack) stack)
					  ((eq? message 'operations) the-ops)
					  ((eq? message 'trace-on) (set! trace-on? #t))
					  ((eq? message 'trace-off) (set! trace-on? #f))
					  ((eq? message 'reg-trace-on) (set! reg-trace-on? #t))
					  ((eq? message 'reg-trace-off) (set! reg-trace-on? #f))
					  ((eq? message 'get-reg-trace-on) reg-trace-on?)
					  ((eq? message 'set-breakpoint) set-breakpoint)
					  ((eq? message 'get-breakpoints) breakpoints)
					  ((eq? message 'proceed-machine) (execute))
					  ((eq? message 'cancel-breakpoint) cancel-breakpoint)
					  ((eq? message 'save-machine-info) (lambda (m text) (set! controller-text text) (set! machine m)))
					  (else (error " Unknown request : MACHINE " message))))
			dispatch)))


(define (start machine) (machine 'start))
(define (proceed-machine machine) (machine 'proceed-machine))
(define (trace-on machine) (machine 'trace-on))
(define (trace-off machine) (machine 'trace-off))
(define (reg-trace-on machine) (machine 'reg-trace-on))
(define (reg-trace-off machine) (machine 'reg-trace-off))
(define (get-inst-count machine) (machine 'get-instruction-counter))
(define (reset-inst-count machine) (machine 'reset-instruction-counter))
(define (get-register-contents machine register-name)
	(get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
	(set-contents! (get-register machine register-name) value (machine 'get-reg-trace-on))
	'done)

(define (set-breakpoint machine label count)
	((machine 'set-breakpoint) label count))
(define (cancel-breakpoint machine label count)
	((machine 'cancel-breakpoint) label count))

(define (get-register machine reg-name)
	((machine 'get-register) reg-name))

(define (assemble controller-text machine)
	(extract-labels
		machine
		controller-text
		'*unassigned*
		1
		(lambda (insts labels)
			(update-insts! insts labels machine)
			insts)))

(define (extract-labels machine text latest-label inst-count-from-label receive)
	(if (null? text)
		(receive '() '())
		(let ((next-inst (car text)))
			; (newline)
			; (display "latest-label: ") (display latest-label) (newline)
			; (display "inst-count-from-label: ") (display inst-count-from-label) (newline)
			; (display "next-inst: ") (display next-inst) (newline)
			; (newline)
			(extract-labels
				machine
				(cdr text)
				(if (symbol? next-inst) next-inst latest-label)
				(if (symbol? next-inst) 1 (+ 1 inst-count-from-label))
				(lambda (insts labels)
					(if (symbol? next-inst)
						(let ((insts (cons (make-instruction (list 'label next-inst)) insts)))
							(receive
								insts
								(cons (make-label-entry next-inst insts) labels)))
						(let ((insts
							(if (breakpoint? machine latest-label inst-count-from-label)
								(cons (make-instruction (list 'breakpoint)) insts)
								insts)))
							(receive
								(cons (make-instruction next-inst) insts)
								labels))))))))

(define (breakpoint? machine label count)
	(let ((breakpoints (machine 'get-breakpoints)))
		; (display "breakpoints: ") (display breakpoints) (newline)
		(not (null?
			(filter
				(lambda (b) (and (eq? (car b) label) (= (cadr b) count)))
				breakpoints)))))

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
		  ((eq? (car inst) 'label)
			(make-label inst pc))
		  ((eq? (car inst) 'breakpoint)
			(make-breakpoint inst machine labels ops pc))
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
				(set-contents! target (value-proc) (machine 'get-reg-trace-on))
				(advance-pc pc)))))

(define (assign-reg-name assign-instruction)
	(cadr assign-instruction))
(define (assign-value-exp assign-instruction)
	(cddr assign-instruction))

(define (advance-pc pc)
	(set-contents! pc (cdr (get-contents pc)) #f))

(define (make-test inst machine labels operations flag pc)
	(let ((condition (test-condition inst)))
		(if (operation-exp? condition)
			(let ((condition-proc
				(make-operation-exp
					condition machine labels operations)))
				(lambda ()
					(set-contents! flag (condition-proc) (machine 'get-reg-trace-on))
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
						(set-contents! pc insts #f)
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
					(lambda () (set-contents! pc insts #f))))
			((register-exp? dest)
				(let ((reg (get-register
					machine
					(register-exp-reg dest))))
					(lambda ()
						(set-contents! pc (get-contents reg) #f))))
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
			(set-contents! reg (pop stack) (machine 'get-reg-trace-on))
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

(define (make-label inst pc)
	(lambda ()
		(newline)
		(display (cadr inst))
		(display ":")
		(advance-pc pc)))

(define (make-breakpoint inst machine labels operations pc)
	(lambda ()
		(newline)
		(display "make-breakpoint")
		(advance-pc pc)))

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

(define fact-machine
	(make-machine
		'(n val continue)
		(list
			(list '= =)
			(list '- -)
			(list '* *))
		'((assign continue (label fact-done))
		  fact-loop
			(test (op =) (reg n) (const 1))
			(branch (label base-case))
			(save continue)
			(save n)
			(assign n (op -) (reg n) (const 1))
			(assign continue (label after-fact))
			(goto (label fact-loop))
		  after-fact
			(restore n)
			(restore continue)
			(assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
			(goto (reg continue)) ;return to caller
		  base-case
			(assign val (const 1))
			(goto (reg continue)) ;base case: 1! = 1
		  fact-done)))

(set-register-contents! fact-machine 'n 5)
(set-breakpoint fact-machine 'fact-loop 6)

(newline)
(trace-on fact-machine)
; (reg-trace-on fact-machine)
(start fact-machine)

(get-register-contents fact-machine 'n)
(set-register-contents! fact-machine 'n 1)
(get-register-contents fact-machine 'n)
(proceed-machine fact-machine)

(newline)

(get-register-contents fact-machine 'val)
(display "first end")
(newline)
(newline)
(trace-off fact-machine)
(cancel-breakpoint fact-machine 'fact-loop 6)
; (cancel-all-breakpoint fact-machine) (set breakpoints '())
(set-register-contents! fact-machine 'n 5)
(start fact-machine)
(newline)
(get-register-contents fact-machine 'val)
