#lang sicp

; util ai code
(define (insert element sorted-list)
  (cond
    [(null? sorted-list) (list element)] ; ソート済みリストが空なら要素をそのまま追加
    [(string<? (symbol->string element) (symbol->string (car sorted-list)))
     (cons element sorted-list)]         ; 要素が先頭より小さければ先頭に挿入
    [else (cons (car sorted-list)        ; そうでなければ先頭を維持し再帰
                (insert element (cdr sorted-list)))]))

(define (insertion-sort lst)
  (if (null? lst)
      '()                               ; 入力リストが空なら空リストを返す
      (insert (car lst) (insertion-sort (cdr lst))))) ; 1つ目をソート済みに挿入
;

(define (make-machine register-names ops controller-text)
	(let ((machine (make-new-machine)))
		(for-each
			(lambda (register-name)
				((machine 'allocate-register) register-name))
			register-names)
		((machine 'install-operations) ops)
		((machine 'install-instruction-sequence)
		 (assemble controller-text machine))
		((machine 'sort-insts))
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
	(let ((s '()))
		(define (push x) (set! s (cons x s)))
		(define (pop)
			(if (null? s)
				(error " Empty stack : POP")
				(let ((top (car s)))
					(set! s (cdr s))
					top)))
		(define (initialize) (set! s '()) 'done)
		(define (dispatch message)
			(cond ((eq? message 'push) push)
				  ((eq? message 'pop) (pop))
				  ((eq? message 'initialize) (initialize))
				  (else (error " Unknown request : STACK " message))))
		dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
	(let ((pc (make-register 'pc))
		  (flag (make-register 'flag))
		  (stack (make-stack))
		  (the-instruction-sequence '())
		  (unique-insts '())
		  (unique-entrypoints '())
		  (unique-regs-save-restore '())
		  (unique-regs-assigned '()))
		(let ((the-ops (list (list 'initialize-stack (lambda () (stack 'initialize)))))
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
			(define (set-unique-insts inst)
				(if (not (memq inst unique-insts))
					(set! unique-insts
						(cons inst unique-insts)))
				'done-set-unique-inst)
			(define (sort-insts)
				(set! unique-insts
					(insertion-sort unique-insts)) ; racketの関数はsicp言語だと使えない。
				'done)
			(define (set-unique-entrypoint entrypoint)
				(let ((entrypoint-name (cadr entrypoint)))
					(if (and (eq? (car entrypoint) 'reg)
							(not (memq entrypoint-name unique-entrypoints)))
						(set! unique-entrypoints
							(cons entrypoint-name unique-entrypoints))))
				'done-set-unique-entrypoint)
			(define (set-unique-reg-save-restore reg)
				(if (not (memq reg unique-regs-save-restore))
					(set! unique-regs-save-restore
						(cons reg unique-regs-save-restore)))
				'done-set-unique-reg-save-restore)
			(define (set-unique-reg-assigned reg original)
				(let ((originals (assoc reg unique-regs-assigned)))
					(if originals
						(set-cdr! originals (cons original (cdr originals)))
						(set! unique-regs-assigned
							(cons (list reg original) unique-regs-assigned))))
				'done-set-unique-reg-assigned)
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
					; set inst information
					  ((eq? message 'set-unique-insts) set-unique-insts)
					  ((eq? message 'set-unique-entrypoint) set-unique-entrypoint)
					  ((eq? message 'set-unique-reg-save-restore) set-unique-reg-save-restore)
					  ((eq? message 'set-unique-reg-assigned) set-unique-reg-assigned)
					; get inst information
					  ((eq? message 'get-unique-insts) unique-insts)
					  ((eq? message 'get-unique-entrypoint) unique-entrypoints)
					  ((eq? message 'get-unique-reg-save-restore) unique-regs-save-restore)
					  ((eq? message 'get-unique-reg-assigned) unique-regs-assigned)
					  ((eq? message 'sort-insts) sort-insts)
					  (else (error " Unknown request : MACHINE " message))))
			dispatch)))


(define (start machine) (machine 'start))
(define (get-insts machine) (machine 'get-unique-insts))
(define (get-entrypoints machine) (machine 'get-unique-entrypoint))
(define (get-regs-stacked machine) (machine 'get-unique-reg-save-restore))
(define (get-regs-assigned machine) (machine 'get-unique-reg-assigned))
(define (get-register-contents machine register-name)
	(get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
	(set-contents! (get-register machine register-name) value)
	'done)

(define (get-register machine reg-name)
	((machine 'get-register) reg-name))

(define (assemble controller-text machine)
	(extract-labels
		machine
		controller-text
		(lambda (insts labels)
			(update-insts! insts labels machine)
			insts)))

(define (extract-labels machine text receive)
	(if (null? text)
		(receive '() '())
		(extract-labels
			machine
			(cdr text)
			(lambda (insts labels)
				(let ((next-inst (car text)))
					(if (symbol? next-inst)
						(receive
							insts
							(cons (make-label-entry next-inst insts) labels))
						(begin
							(let ((inst (car next-inst)))
								((machine 'set-unique-insts) (car next-inst))
								(cond 
									((eq? inst 'goto)
										((machine 'set-unique-entrypoint) (cadr next-inst)))
									((or (eq? inst 'save) (eq? inst 'restore))
										((machine 'set-unique-reg-save-restore) (cadr next-inst)))
									((eq? inst 'assign)
										((machine 'set-unique-reg-assigned) (cadr next-inst) (cddr next-inst)))))
							(receive
								(cons (make-instruction next-inst) insts)
								labels))))))))

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



(define fib-machine
	(make-machine
		'(n val continue)
		(list
			(list '< <)
			(list '- -)
			(list '+ +))
		'((assign continue (label fib-done))
		  fib-loop
			(test (op <) (reg n) (const 2))
			(branch (label immediate-answer))
			;; Fib(n−1) を求める準備
			(save continue)
			(assign continue (label afterfib-n-1))
			(save n) ; n の古い値を保存
			(assign n (op -) (reg n) (const 1)) ; n を n-1 で上書き
			(goto (label fib-loop)) ; 再帰呼び出しの実⾏
		  afterfib-n-1 ; リターン時に Fib(n−1) は val に⼊っている
			(restore n)
			; (restore continue)
			;; Fib(n−2) を求める準備
			(assign n (op -) (reg n) (const 2))
			; (save continue)
			(assign continue (label afterfib-n-2))
			(save val) ; Fib(n−1) を保存
			(goto (label fib-loop))
		  afterfib-n-2 ; リターン時に Fib(n−2) は val に⼊っている
			(assign n (reg val)) ; n には Fib(n−2) が⼊る
			(restore val) ; valには Fib(n−1) が⼊る
			(restore continue) 
			(assign val ; Fib(n−1) + Fib(n−2)
				(op +) (reg val) (reg n))
			(goto (reg continue)) ; 呼び出し元に戻る、答えはval の中
		  immediate-answer
			(assign val (reg n))
			(goto (reg continue))
		  fib-done
		)))

(set-register-contents! fib-machine 'n 6)
(get-insts fib-machine)
(get-entrypoints fib-machine)
(get-regs-stacked fib-machine)
(get-regs-assigned fib-machine)
(start fib-machine)
(get-register-contents fib-machine 'val)


; 階乗マシン
; (controller
; (assign continue (label fact-done)) ;set up final return address
; fact-loop
; (test (op =) (reg n) (const 1))
; (branch (label base-case))
; ;; Set up for the recursive call by saving n and continue.
; ;; Set up continue so that the computation will continue
; ;; at after-fact when the subroutine returns.
; (save continue)
; (save n)
; (assign n (op -) (reg n) (const 1))
; (assign continue (label after-fact))
; (goto (label fact-loop))
; after-fact
; (restore n)
; (restore continue)
; (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
; (goto (reg continue)) ;return to caller
; base-case
; (assign val (const 1)) (goto (reg continue)) ;base case: 1! = 1
; ;return to caller
; fact-done)