#lang sicp

(#%require "./query-util.rkt")

(define input-prompt "~~~ Query input :")
(define output-prompt "~~~ Query results :")

(define (query-driver-loop)
	(define (internal-loop try-again)
		(prompt-for-input input-prompt)
		(let ((input (read)))
			(if (eq? input 'try-again)
				(try-again)
				(let ((q (query-syntax-process input)))
					(cond ((assertion-to-be-added? q)
							(add-rule-or-assertion! (add-assertion-body q))
							(newline) (display "Assertion added to data base.")
							(query-driver-loop))
						  (else
							(newline) (display output-prompt)
							(newline) (display ";;; Starting a new problem")
							(qeval q (singleton-stream '())
								;; success
								(lambda (frame next-alternative)
									(newline)
									(display
										(instantiate
											q
											frame
											(lambda (v f)
												(contract-question-mark v))))
									(internal-loop next-alternative))
								;; failure
								(lambda ()
									(announce-output ";;; There are no more values of")
									(user-print input)
									(query-driver-loop)))))
							(query-driver-loop)))))
	(internal-loop
		(lambda ()
			(newline) (display ";;; There is no current problem ")
			(query-driver-loop))))

(define (instantiate exp frame unbound-var-handler)
	(define (copy exp)
		(cond
			((var? exp)
				(let ((binding (binding-in-frame exp frame)))
					(if binding
						(copy (binding-value binding))
						(unbound-var-handler exp frame))))
			((pair? exp)
				(cons (copy (car exp)) (copy (cdr exp))))
			(else exp)))
	(copy exp))

; 4.4.4.2

(define (qeval query frame-stream succeed fail)
	(let ((qproc (get (type query) 'qeval)))
		(if qproc
			(qproc (contents query) frame-stream succeed fail)
			(simple-query query frame-stream succeed fail))))

(define (apply-succeed streams succeed fail)
	(if (stream-null? streams)
		(fail)
		(succeed
			(stream-car streams)
			(lambda () (apply-succeed (stream-cdr streams) succeed fail)))))

(define (simple-query query-pattern frame-stream succeed fail)
	(let ((streams
		(stream-flatmap
			(lambda (frame)
				(stream-append-delayed
					(find-assertions query-pattern frame)
					(delay (apply-rules query-pattern frame))))
			frame-stream)))
		(apply-succeed streams succeed fail)))

; streamを組み合わせる時にsucceedされる場合
; (define (simple-query query-pattern frame-stream succeed fail)
; 	(let ((ret-singleton-stream
; 		(stream-flatmap
; 			(lambda (frame)
; 				; (display-stream (find-assertions query-pattern frame))
; 				(stream-append-delayed
; 					(find-assertions query-pattern frame)
; 					(delay (apply-rules query-pattern frame))))
; 			frame-stream
; 			succeed fail)))
; 		(if (null? ret-singleton-stream)
; 			(fail)
; 			(succeed (stream-car ret-singleton-stream) fail))))

(define (conjoin conjuncts frame-stream)
	(if (empty-conjunction? conjuncts)
		frame-stream
		(conjoin (rest-conjuncts conjuncts)
				 (qeval (first-conjunct conjuncts) frame-stream))))
(put 'and 'qeval conjoin)

; streamを組み合わせる時にsucceedされる場合
; (define (conjoin conjuncts frame-stream succeed fail)
; 	(define (filter conj frame-s)
; 		(qeval (first-conjunct conj) frame-s
; 			(lambda (frame next) ;succeed
; 				(if (empty-conjunction? (rest-conjuncts conj))
; 					succeed
; 					(filter (rest-conjuncts conj) (singleton-stream frame))))
; 			(lambda () the-empty-stream)))
; 	(if (empty-conjunction? conjuncts)
; 		(succeed frame-stream fail) ; (and)
; 		(filter conjuncts frame-stream) ; other
; 	)
; )

(define (disjoin disjuncts frame-stream)
	(if (empty-disjunction? disjuncts)
		the-empty-stream
		(interleave-delayed
			(qeval (first-disjunct disjuncts) frame-stream)
			(delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))
(put 'or 'qeval disjoin)

(define (negate operands frame-stream)
	(stream-flatmap
		(lambda (frame)
			(if (stream-null?
				(qeval (negated-query operands)
						(singleton-stream frame)))
				(singleton-stream frame)
				the-empty-stream))
		frame-stream))
(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
	(stream-flatmap
		(lambda (frame)
			(if (execute
					(instantiate
						call
						frame
						(lambda (v f)
							(error "Unknown pat var: LISP-VALUE " v))))
				(singleton-stream frame)
				the-empty-stream))
		frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
	(apply (eval (predicate exp) user-initial-environment)
		   (args exp)))

(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

; 4.4.4.3

(define (find-assertions pattern frame)
	(stream-flatmap
		(lambda (datum) (check-an-assertion datum pattern frame))
		(fetch-assertions pattern frame)))

; streamを組み合わせる時にsucceedされる場合
; (define (find-assertions pattern frame)
; 	(stream-flatmap
; 		(lambda (datum) (check-an-assertion datum pattern frame))
; 		(fetch-assertions pattern frame)
; 		(lambda (frame next)
; 			(cons-stream frame (next)))
; 		(lambda () the-empty-stream)))

(define (check-an-assertion assertion query-pat query-frame)
	(let ((match-result (pattern-match query-pat assertion query-frame)))
		(if (eq? match-result 'failed)
			the-empty-stream
			(singleton-stream match-result))))

(define (pattern-match pat dat frame)
	(cond ((eq? frame 'failed) 'failed)
		  ((equal? pat dat) frame)
		  ((var? pat) (extend-if-consistent pat dat frame))
		  ((and (pair? pat) (pair? dat))
			(pattern-match
				(cdr pat)
				(cdr dat)
				(pattern-match (car pat) (car dat) frame)))
		  (else 'failed)))

(define (extend-if-consistent var dat frame)
	(let ((binding (binding-in-frame var frame)))
		(if binding
			(pattern-match (binding-value binding) dat frame)
			(extend var dat frame))))

; 4.4.4.4

(define (apply-rules pattern frame)
	(stream-flatmap
		(lambda (rule) (apply-a-rule rule pattern frame))
		(fetch-rules pattern frame)))

; streamを組み合わせる時にsucceedされる場合
; (define (apply-rules pattern frame succeed fail)
; 	(stream-flatmap
; 		(lambda (rule) (apply-a-rule rule pattern frame))
; 		(fetch-rules pattern frame)
; 		(lambda (frame next)
; 			(cons-stream frame (next)))
; 		(lambda () the-empty-stream)))

(define (apply-a-rule rule query-pattern query-frame)
	(let ((clean-rule (rename-variables-in rule)))
		(let ((unify-result (unify-match query-pattern (conclusion clean-rule) query-frame)))
			(if (eq? unify-result 'failed)
				the-empty-stream
				(qeval
					(rule-body clean-rule)
					(singleton-stream unify-result))))))

(define (rename-variables-in rule)
	(let ((rule-application-id (new-rule-application-id)))
		(define (tree-walk exp)
			(cond ((var? exp) (make-new-variable exp rule-application-id))
				  ((pair? exp) (cons (tree-walk (car exp)) (tree-walk (cdr exp))))
				  (else exp)))
		(tree-walk rule)))

(define (unify-match p1 p2 frame)
	(cond ((eq? frame 'failed) 'failed)
		  ((equal? p1 p2) frame)
		  ((var? p1) (extend-if-possible p1 p2 frame))
		  ((var? p2) (extend-if-possible p2 p1 frame)) ; ***
		  ((and (pair? p1) (pair? p2))
			(unify-match
				(cdr p1)
				(cdr p2)
				(unify-match (car p1) (car p2) frame)))
		  (else 'failed)))

(define (extend-if-possible var val frame)
	(let ((binding (binding-in-frame var frame)))
		(cond (binding (unify-match (binding-value binding) val frame))
			  ((var? val) ; ***
				(let ((binding (binding-in-frame val frame)))
					(if binding
						(unify-match var (binding-value binding) frame)
						(extend var val frame))))
			  ((depends-on? val var frame) ; ***
				'failed)
			  (else (extend var val frame)))))

(define (depends-on? exp var frame)
	(define (tree-walk e)
		(cond 
			((var? e)
				(if (equal? var e)
					true
					(let ((b (binding-in-frame e frame)))
						(if b
							(tree-walk (binding-value b))
							false))))
			((pair? e)
				(or (tree-walk (car e)) (tree-walk (cdr e))))
			(else false)))
	(tree-walk exp))

; 4.4.4.5

(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
	(if (use-index? pattern)
		(get-indexed-assertions pattern)
		(get-all-assertions)))
(define (get-all-assertions) THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
	(get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
	(let ((s (get key1 key2)))
		(if s s the-empty-stream)))

(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
	(if (use-index? pattern)
		(get-indexed-rules pattern)
		(get-all-rules)))
(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
	(stream-append
		(get-stream (index-key-of pattern) 'rule-stream)
		(get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
	(if (rule? assertion)
		(add-rule! assertion)
		(add-assertion! assertion)))

(define (add-assertion! assertion)
	(store-assertion-in-index assertion)
	(let ((old-assertions THE-ASSERTIONS))
		(set! THE-ASSERTIONS
			(cons-stream assertion old-assertions))
		'ok))
(define (add-rule! rule)
	(store-rule-in-index rule)
	(let ((old-rules THE-RULES))
		(set! THE-RULES (cons-stream rule old-rules))
		'ok))

(define (store-assertion-in-index assertion)
	(if (indexable? assertion)
		(let ((key (index-key-of assertion)))
			(let ((current-assertion-stream (get-stream key 'assertion-stream)))
				(put key
					'assertion-stream
					(cons-stream assertion current-assertion-stream))))))
	(define (store-rule-in-index rule)
		(let ((pattern (conclusion rule)))
			(if (indexable? pattern)
				(let ((key (index-key-of pattern)))
					(let ((current-rule-stream (get-stream key 'rule-stream)))
						(put key
							'rule-stream
							(cons-stream rule current-rule-stream)))))))

(define (indexable? pat)
	(or (constant-symbol? (car pat))
		(var? (car pat))))

(define (index-key-of pat)
	(let ((key (car pat)))
		(if (var? key) '? key)))

(define (use-index? pat) (constant-symbol? (car pat)))

; 4.4.4.6

(define (stream-append-delayed s1 delayed-s2)
	(if (stream-null? s1)
		(force delayed-s2)
		(cons-stream
			(stream-car s1)
			(stream-append-delayed (stream-cdr s1) delayed-s2))))
(define (interleave-delayed s1 delayed-s2)
	(if (stream-null? s1)
		(force delayed-s2)
		(cons-stream
			(stream-car s1)
			(interleave-delayed (force delayed-s2) (delay (stream-cdr s1))))))

(define (stream-flatmap proc s) (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
	(if (stream-null? stream)
		the-empty-stream
		(interleave-delayed (stream-car stream) (delay (flatten-stream (stream-cdr stream))))))

; streamを組み合わせる時にsucceedされる場合
; (define (interleave-delayed s1 delayed-s2 succeed fail)
; 	(if (stream-null? s1)
; 		(force delayed-s2) ; stream
; 		(succeed
; 			(stream-car s1)
; 			(lambda () ;next empty-stream or 
; 				(let ((ret (interleave-delayed (force delayed-s2) (delay (stream-cdr s1)) succeed fail)))
; 					(if (not (procedure? ret))
; 						(succeed
; 							(stream-car ret) )))
; 						))))

; (define (stream-flatmap proc s succeed fail)
; 	(flatten-stream (stream-map proc s) succeed fail))
; (define (flatten-stream stream succeed fail)
; 	(if (stream-null? stream)
; 		the-empty-stream
; 		(interleave-delayed (stream-car stream) (delay (flatten-stream (stream-cdr stream) succeed fail)) succeed fail)))

(define (singleton-stream x) (cons-stream x the-empty-stream))

; 4.4.4.7

(define (type exp)
	(if (pair? exp)
		(car exp)
		(error " Unknown expression TYPE " exp)))
(define (contents exp)
	(if (pair? exp)
		(cdr exp)
		(error " Unknown expression CONTENTS " exp)))

(define (assertion-to-be-added? exp) (eq? (type exp) 'assert!))
(define (add-assertion-body exp) (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement) (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule) (if (null? (cddr rule)) '(always-true) (caddr rule)))

(define (query-syntax-process exp) (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
	(cond ((pair? exp)
			(cons (map-over-symbols proc (car exp))
				  (map-over-symbols proc (cdr exp))))
		  ((symbol? exp) (proc exp))
		  (else exp)))
(define (expand-question-mark symbol)
	(let ((chars (symbol->string symbol)))
		(if (string=? (substring chars 0 1) "?")
			(list '?
				(string->symbol
					(substring chars 1 (string-length chars))))
			symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)
(define (new-rule-application-id)
	(set! rule-counter (+ 1 rule-counter))
	rule-counter)
(define (make-new-variable var rule-application-id)
	(cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
	(string->symbol
		(string-append "?"
			(if (number? (cadr variable))
				(string-append
					(symbol->string (caddr variable))
					"-"
					(number->string (cadr variable)))
				(symbol->string (cadr variable))))))

; 4.4.4.8

(define (make-binding variable value) (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame) (assoc variable frame))
(define (extend variable value frame)
	(cons (make-binding variable value) frame))

(define (user-print query) (display query))

(query-driver-loop)

#|

DEBUG

(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(and (supervisor ?x ?y)
	 (job ?x (computer programmer)))
(job ?x (computer programmer))
(job ?x (computer . ?type))

方針
1. try-againで"hello world"

2. simple-queryで一つのクエリが選ばれた場合、速攻で返す。
	値が生成されなかったらtry-againする。

3. 回答が存在しなかった場合には、There are no more values of.

考察
この節で構築した仕組みの多くは⾮決定性探索とバックトラックに含まれていたということに気づくはずだ。
->delayとforceのことだろう。

次回 ambeval
2時間経っても光が見えなかったら答えを見よう。

Q.失敗はどう判定すればいい？
instantiateの部分ではクエリが成功している前提。
最終のqevalのクエリ結果が何も含まれていない時は失敗

Q.contract-...?はどこで使われている？
(or (supervisor ?x ?y)
        (not (job ?y ?z)))   

simple-queryでは何らかのframeを取得できなければfail
the-empty-frameを返しているところでfail

|#