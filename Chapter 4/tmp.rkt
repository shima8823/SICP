#lang sicp

;; permanet-set! 実装すみ
;; if-fail 実装すみ
;; ramb 実装すみ

;; Amb評価器

(define apply-in-underlying-scheme apply)   ;;脚注17

;;駆動ループ 4.1.4
(define input-prompt "~~~ Amb-Eval input:")
(define output-prompt "~~~ Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment ;;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative)) ;;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     ))
      (display object)))

;;環境

(define primitive-procedures
  (list (list '* *)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '< <)
        (list '<= <=)
        (list '= =)
        (list '> >)
        (list '>= >=)
;;      (list 'apply apply)
        (list 'assoc assoc)
        (list 'atan atan)
        (list 'cadr cadr)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'caar caar)
        (list 'cadar cadar)
        (list 'cos cos)
        (list 'display display)
        (list 'eq? eq?)
        (list 'error error)
;;      (list 'eval eval)
        (list 'list list)
        (list 'list-ref list-ref)
        (list 'list-tail list-tail)
        (list 'log log)
        (list 'max max)
        (list 'member member)
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
        (list 'vector-ref vector-ref)
        (list 'vector-set! vector-set!)
        (list 'sqrt sqrt)
        (list 'floor floor)
        (list 'integer? integer?)
        (list 'inexact->exact inexact->exact)
        (list 'even? even?)
        (list 'abs abs)
        (list 'append append)
        (list 'map map)
        (list 'length length)

        ))

(define the-empty-environment '())

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (make-frame variables values)
  (cons variables values))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (first-frame env) (car env))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))


(define (enclosing-environment env) (cdr env))



(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;; 4.1.2 evalの下請け手続き

;; 4.1.2 自己評価式
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; 4.1.2 変数
(define (variable? exp) (symbol? exp))

;; 4.1.2 クォート式
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; 4.1.2 代入
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (permanent-assignment? exp)          ;;ex4.51
  (tagged-list? exp 'permanent-set!))


(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; 4.1.2 定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ;formal parameter
                   (cddr exp)))) ;body

;; 4.1.2 lambda式
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; 4.1.2 条件式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (if-fail? exp) (tagged-list? exp 'if-fail)) ;;ex4.52

(define (if-fail-first exp)                         ;;ex4.52
  (cadr exp))

(define (if-fail-second exp)                        ;;ex4.52
  (caddr exp))


;; 4.1.2 begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; 4.1.2 手続き作用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))


;;(define (require? exp) (tagged-list? exp 'require)) ;ex4.54

;;(define (require-predicate exp) (cadr exp))         ;ex4.54

;; 4.1.2 導出された式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))


(define (expand-clauses clauses)
  (if (null? clauses)
      'false                           ;no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (eq? (cadr first) '=>)                                   ;ex4.05
            (let ((temp `(let ((val ,(cond-predicate first)))       ;ex4.05
               (if val (,(caddr first) val)                         ;ex4.05
                  ,(expand-clauses rest)))))
             (display temp) (newline) temp)
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest)))))))

;(define (expand-clauses clauses)
;  (if (null? clauses)
;      'false                           ;no else clause
;      (let ((first (car clauses))
;            (rest (cdr clauses)))
;        (if (cond-else-clause? first)
;            (if (null? rest)
;                (sequence->exp (cond-actions first))
;                (error "ELSE clause isn't last -- COND->IF"
;                       clauses))
;            (make-if (cond-predicate first)
;                     (sequence->exp (cond-actions first))
;                     (expand-clauses rest))))))

;; 4.1.1 手続きの引数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; 4.1.1 条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; 4.1.1 並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; 4.1.1 代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; 4.1.7 eval
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
                                             ;;ex4.51
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))    ;; ex4.52
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((and? exp) (analyze (and->if exp)))     ;ex4.04
        ((or? exp) (analyze (or->if exp)))       ;ex4.04
        ((let? exp) (analyze (let->combination exp)))

        ((amb? exp) (newline) (analyze-amb exp))
        ((ramb? exp) (newline) (analyze-ramb exp))   ;;ex4.50
;;      ((require? exp) (newline) (analyze-require exp))   ;ex4.54

        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ;; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ;; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-permanent-assignment exp)    ;;ex4.51
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (fail2))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))


(define (analyze-if-fail exp)                       ;;ex4.52
  (let ((fproc (analyze (if-fail-first exp)))
        (sproc (analyze (if-fail-second exp))))
    (lambda (env succeed fail)
     (fproc env succeed (lambda () (sproc env succeed fail))))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
;;;(display (list 'as exps)) (newline)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;(define (analyze-require exp)
;;  (let ((pproc (analyze (require-predicate exp))))
;;    (lambda (env succeed fail)
;;      (pproc env
;;             (lambda (pred-value fail2)
;;               (if (not pred-value)
;;                   (fail)
;;                   (succeed 'ok fail2)))
;;             fail))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;; ex4.50
(define (analyze-ramb exp)
  (define (list-rem list n)
    (if (= n 0) (cdr list)
        (cons (car list) (list-rem (cdr list) (- n 1)))))
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((nth (random (length choices))))
              (let ((next (list-ref choices nth))
                    (rest (list-rem choices nth)))
                (next env succeed
                           (lambda ()
                             (try-next rest)))))))
      (try-next cprocs))))

;; 4.1.1 apply

(define (apply-- procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


;;; representing procedures

;; 4.1.3 条件のテスト
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; 4.1.3 手続きの表現
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;;amb
(define (amb? exp) (tagged-list? exp 'amb))

;;ramb ex4.50
(define (ramb? exp) (tagged-list? exp 'ramb))

(define (amb-choices exp) (cdr exp))

;; 4.1.3 環境に対する操作

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


;;ex4.04
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (and->if exp)
  (expand-conjuncts (conjuncts exp)))
(define (conjuncts exp) (cdr exp))

(define (expand-conjuncts conjuncts)
  (cond ((null? conjuncts) 'false)
        ((null? (cdr conjuncts))
         (make-if (car conjuncts) (car conjuncts) 'false))
        (else (make-if (car conjuncts) 
                       (expand-conjuncts (cdr conjuncts))
                       'false))))

(define (or->if exp)
  (expand-disjuncts (disjuncts exp)))
(define (disjuncts exp) (cdr exp))

(define (expand-disjuncts disjuncts)
  (cond ((null? disjuncts) 'true)
        ((null? (cdr disjuncts))
         (make-if (car disjuncts) (car disjuncts) 'false))
        (else (make-if (car disjuncts)
                       (car disjuncts)
                       (expand-disjuncts (cdr disjuncts))))))
;; ex4.06 ex4.08 ex4.22

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (if (symbol? (cadr exp))
  (let ((tag (cadr exp)) (bindings (caddr exp)) (body (cdddr exp)))   ;ex4.08
      (list (list 'lambda '()                                         ;ex4.08
              (cons 'define (cons (cons tag (map car bindings)) body));ex4.08
               (cons tag (map cadr bindings)))))                      ;ex4.08
  (let ((bindings (cadr exp)) (body (cddr exp)))
   (cons (make-lambda (map car bindings) body)
     (map cadr bindings)))))


;; ex4.07

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (nested-lets bindings body)
    (if (null? (cdr bindings))
         (cons (make-lambda (list (caar bindings)) body) (cdar bindings))
        (cons (make-lambda (list (caar bindings)) 
          (list (nested-lets (cdr bindings) body))) (cdar bindings))))
  (let ((bindings (cadr exp)) (body (cddr exp)))
    (nested-lets bindings body)))

;; ex4.20
(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ((bindings (cadr exp)) (body (cddr exp)))
    (cons 'let (cons 
     (map (lambda (x) (list (car x) ''())) bindings)
       (append (map (lambda (x) (cons 'set! x)) bindings) body)))))


;; 4.1.4 対話開始

; (define the-global-environment (setup-environment))

; (display (string #\bel))
; (newline)(newline)
; (display
;   "CAUTION: the original apply was removed; mceval cannot be loaded again.")
; (newline)
; (display
;   "USE (driver-loop) to return to metaciucular evaluator.")
; (newline)
(driver-loop)

; (define (safe? k positions)
; 	(display (list 'safe? k positions)) (newline)
