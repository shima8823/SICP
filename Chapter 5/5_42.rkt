#lang sicp

(#%require "util_metacircular.rkt")

(define (compile exp target linkage compile-time-environment)
	(cond
		((self-evaluating? exp)
			(compile-self-evaluating exp target linkage compile-time-environment))
		((quoted? exp)
			(compile-quoted exp target linkage compile-time-environment))
		((variable? exp)
			(compile-variable exp target linkage compile-time-environment))
		((assignment? exp)
			(compile-assignment exp target linkage compile-time-environment))
		((definition? exp)
			(compile-definition exp target linkage compile-time-environment))
		((if? exp)
			(compile-if exp target linkage compile-time-environment))
		((lambda? exp)
			(compile-lambda exp target linkage compile-time-environment))
		((begin? exp)
			(compile-sequence
				(begin-actions exp) target linkage compile-time-environment))
		((cond? exp)
			(compile (cond->if exp) target linkage compile-time-environment))
		((application? exp)
			(compile-application exp target linkage compile-time-environment))
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

(define (compile-self-evaluating exp target linkage compile-time-environment)
	(end-with-linkage linkage
		(make-instruction-sequence '() (list target)
			`((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage compile-time-environment)
	(end-with-linkage linkage
		(make-instruction-sequence '() (list target) 
			`((assign ,target (const ,(text-of-quotation exp)))))))
(define (compile-variable exp target linkage compile-time-environment)
	(end-with-linkage linkage
		(make-instruction-sequence '(env) (list target)
			(let ((lexical-address (find-variable exp compile-time-environment)))
				(if (eq? lexical-address 'not-found)
					; 存在しなかったら直接グローバル環境を⾒に⾏くようにすることもできる。
					; グローバル環境は演算 (op get-global-environment)
					`((assign ,target (op lookup-variable-value) (const ,exp) (reg env)))
					`((assign ,target (op lexical-address-lookup) (const ,lexical-address) (reg env))))
					))))

(define (compile-assignment exp target linkage compile-time-environment)
	(let ((var (assignment-variable exp))
		  (get-value-code
			(compile (assignment-value exp) 'val 'next compile-time-environment)))
		(end-with-linkage linkage
			(preserving '(env)
				get-value-code
			 	(make-instruction-sequence '(env val) (list target)
					(let ((lexical-address (find-variable var compile-time-environment)))
						(if (eq? lexical-address 'not-found)
							`((perform (op set-variable-value!)
										(const ,var)
										(reg val)
										(reg env))
							  (assign ,target (const ok)))
							`((perform (op lexical-address-set!)
										(const ,lexical-address)
										(reg val)
										(reg env))
							  (assign ,target (const ok))))))))))

(define (compile-definition exp target linkage compile-time-environment)
	(let ((var (definition-variable exp))
		  (get-value-code
			(compile (definition-value exp) 'val 'next compile-time-environment)))
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

(define (compile-if exp target linkage compile-time-environment)
	(let ((t-branch (make-label 'true-branch))
		  (f-branch (make-label 'false-branch))
		  (after-if (make-label 'after-if)))
		(let ((consequent-linkage
				(if (eq? linkage 'next) after-if linkage)))
			(let ((p-code (compile (if-predicate exp) 'val 'next compile-time-environment))
				  (c-code (compile (if-consequent exp) target consequent-linkage compile-time-environment))
				  (a-code (compile (if-alternative exp) target linkage compile-time-environment)))
				(preserving '(env continue) p-code
					(append-instruction-sequences
						(make-instruction-sequence '(val) '()
							`((test (op false?) (reg val))
							  (branch (label ,f-branch))))
						(parallel-instruction-sequences
							(append-instruction-sequences t-branch c-code)
							(append-instruction-sequences f-branch a-code))
						after-if))))))

(define (compile-sequence seq target linkage compile-time-environment)
	(if (last-exp? seq)
		(compile (first-exp seq) target linkage compile-time-environment)
		(preserving '(env continue)
			(compile (first-exp seq) target 'next compile-time-environment)
			(compile-sequence (rest-exps seq) target linkage compile-time-environment))))

(define (make-compiled-procedure entry env)
	(list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
	(tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-lambda exp target linkage compile-time-environment)
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
					(compile-lambda-body exp proc-entry compile-time-environment))
				after-lambda))))


(define (compile-lambda-body exp proc-entry compile-time-environment)
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
			(compile-sequence (lambda-body exp) 'val 'return (cons formals compile-time-environment))))) ;change

(define (compile-application exp target linkage compile-time-environment)
	(let ((proc-code (compile (operator exp) 'proc 'next compile-time-environment))
		  (operand-codes
			(map (lambda (operand) (compile operand 'val 'next compile-time-environment))
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

; cdr-recはlist-refでもいいが、error-msgを固有のものにしたい。
(define (lexical-address-lookup lexical-address runtime-env)
	(let ((frame-number (car lexical-address))
		  (displacement-number (cdr lexical-address)))
		(define (cdr-rec count target-list)
			(if (= frame-number 0)
				target-list
				(let ((target-list (cdr target-list)))
					(if (null? target-list)
						(error "Invalid lexical-address LOOKUP" lexical-address)
						(cdr-rec (- count 1) target-list)))))
		(let* ((frame (cdr-rec runtime-env frame-number))
			   (val (cdr (cdr-rec frame displacement-number))))
			(if (eq? val "*unassigned*")
				(error "val is unassigned")
				val))))

; cdr-recはlist-refでもいいが、error-msgを固有のものにしたい。
(define (lexical-address-set! lexical-address val runtime-env)
	(let ((frame-number (car lexical-address))
		  (displacement-number (cdr lexical-address)))
		(define (cdr-rec count target-list)
			(if (= frame-number 0)
				target-list
				(let ((target-list (cdr target-list)))
					(if (null? target-list)
						(error "Invalid lexical-address: SET!" lexical-address)
						(cdr-rec (- count 1) target-list)))))
		(let* ((frame (cdr-rec runtime-env frame-number))
			   (old-val (cdr-rec frame displacement-number)))
			(set-cdr! old-val val))))

(define (find-variable var compile-time-env)
	(define (env-loop frame-count env)
		(define (scan displacement-count vars)
			(cond
				((null? vars)
					(env-loop (+ frame-count 1) (enclosing-environment env)))
				((eq? var (car vars)) (cons frame-count displacement-count))
				(else (scan (+ displacement-count 1) (cdr vars)))))
		(if (eq? env the-empty-environment)
			'not-found
			(let ((frame (first-frame env)))
				(scan
					0
					frame))))
	(env-loop 0 compile-time-env))

(display-insts
(compile
	'((lambda (x y)
		(lambda (a b c d e)
			(set! x y)
			((lambda (y z) (* x y z))
				(* a b x)
				(+ c d x ))))
		3
		4)
	'val
	'next
	'()))

#|
find-variableではlookup-variable-valueと同じく
env自体をcdr, carで一つずつ走査していくがlexical-addressingを使うことによって
lambdaで定義された変数は実行時ではなくコンパイル時に計算されるため実行時間を短縮できる。


(assign proc (op make-compiled-procedure) (label entry1) (reg env))
(goto (label after-lambda2))
entry1
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
(assign val (op make-compiled-procedure) (label entry3) (reg env))
(goto (reg continue))
entry3
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (a b c d e)) (reg argl) (reg env))


(assign val (op lexical-address-lookup) (const (1 . 1)) (reg env)) ;; y
(perform (op lexical-address-set!) (const (1 . 0)) (reg val) (reg env)) ;; x
(assign val (const ok))


(assign proc (op make-compiled-procedure) (label entry5) (reg env))
(goto (label after-lambda6))
entry5
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (y z)) (reg argl) (reg env))
(assign proc (op lookup-variable-value) (const *) (reg env))


(assign val (op lexical-address-lookup) (const (0 . 1)) (reg env)) ;; z
(assign argl (op list) (reg val))
(assign val (op lexical-address-lookup) (const (0 . 0)) (reg env)) ;; y
(assign argl (op cons) (reg val) (reg argl))
(assign val (op lexical-address-lookup) (const (2 . 0)) (reg env)) ;; x
(assign argl (op cons) (reg val) (reg argl))


(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch7))
compiled-branch8
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch7
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call9
after-lambda6
(save continue)
(save proc)
(save env)
(assign proc (op lookup-variable-value) (const +) (reg env))


(assign val (op lexical-address-lookup) (const (1 . 0)) (reg env)) ;; x
(assign argl (op list) (reg val))
(assign val (op lexical-address-lookup) (const (0 . 3)) (reg env)) ;; d
(assign argl (op cons) (reg val) (reg argl))
(assign val (op lexical-address-lookup) (const (0 . 2)) (reg env)) ;; c
(assign argl (op cons) (reg val) (reg argl))


(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch13))
compiled-branch14
(assign continue (label after-call15))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch13
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call15
(assign argl (op list) (reg val))
(restore env)
(save env)
(save argl)
(assign proc (op lookup-variable-value) (const *) (reg env))


(assign val (op lexical-address-lookup) (const (1 . 0)) (reg env)) ;; x
(assign argl (op list) (reg val))
(assign val (op lexical-address-lookup) (const (0 . 1)) (reg env)) ;; b
(assign argl (op cons) (reg val) (reg argl))
(assign val (op lexical-address-lookup) (const (0 . 0)) (reg env)) ;; a
(assign argl (op cons) (reg val) (reg argl))


(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch10))
compiled-branch11
(assign continue (label after-call12))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch10
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call12
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch16))
compiled-branch17
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch16
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
after-call18
after-lambda4
after-lambda2
(assign val (const 4))
(assign argl (op list) (reg val))
(assign val (const 3))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch19))
compiled-branch20
(assign continue (label after-call21))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch19
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call21
|#