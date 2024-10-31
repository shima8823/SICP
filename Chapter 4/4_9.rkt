; return

#lang sicp
(#%require (only racket/base make-base-namespace))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (make-begin seq) (cons 'begin seq))
(define (sequence->exp seq)
	(cond
		((null? seq) seq)
		((last-exp? seq) (first-exp seq))
		(else (make-begin seq))))

#|

(begin
	body
	(true? condtion)
	body
	(true? condtion))

(do
	body
(while condtions))
=(do body condtions)

body
recur
(if (true? condtions)
	body)


sequence->exp
	body
	(set! i (+ i 1))
	(set! sum (+ sum i))

(if (true? (> i 10))
	sum
	whole-body
	)

(do ((i 1 (+ i 1))          ; iを1から始めて、毎回1ずつ増加させる
     (sum 0 (+ sum i)))     ; sumを0から始めて、毎回iを加算する
    ((> i 10) sum)          ; iが10を超えたら終了し、sumを返す
    (display i)(newline))   ; ボディ：現在のiの値を表示

(let ((i 1) (sum 0))
	(define (body)
		(sequence->exp
			body
			(set! i (+ i 1))
			(set! sum (+ sum i))))
	(define (loop)
		(if (true? (> i 10))
			sum
			(sequence->exp
				(body)
				(loop)))))
	(sequence->exp
		(body)
		(loop)))
|#



; (define (do? exp) (tagged-list? exp 'do))
(define (do-variables exp) (cadr exp))
(define (do-variable-name v) (car v))
(define (do-variable-init v) (cadr v))
(define (do-variable-set v) (caddr v))
(define (do-body exp) (cadddr exp))
(define (do-condtion exp) (car (caddr exp)))
(define (do-return-value exp) (cadr (caddr exp)))

(define (do-make-body body variables)
	(display (list
		'define '(body) 
		(list
			'sequence->exp
			(cons
				body
				(map
					(lambda (v)
						(list 'set! (do-variable-name v) (do-variable-set v)))
					variables))
			)
		))(newline)
	(list
		'define '(body) 
		(list
			'sequence->exp
			(cons
				body
				(map
					(lambda (v)
						(list 'set! (do-variable-name v) (do-variable-set v)))
					variables))
			)
		))

(define (do-make-loop body condtion return-value)
	(list
		'define '(loop) 
		(list
			'if
			; (list 'true? condtion) 
			condtion
			return-value
			(list
				'sequence->exp
				(list body '(loop)))
			)
		))

(define (do->let exp)
	
	(list
		; (sequence->exp (map (lambda (v) (list 'define (do-variable-name v) (do-variable-init v))) (do-variables exp)))
		'let
		(map (lambda (v) (list (do-variable-name v) (do-variable-init v))) (do-variables exp))
		(do-make-body (do-body exp) (do-variables exp))
		(do-make-loop (do-body exp) (do-condtion exp) (do-return-value exp))
		(list 'sequence->exp (list (do-body exp) '(loop)))
		))

(do->let
		'(do ((i 1 (+ i 1))
			(sum 0 (+ sum i)))
			((> i 10) sum)
			(display i)(newline)
			))

(define ns (make-base-namespace))
(eval '(define (sequence->exp seq)
		(display seq)(newline)
         (cond ((null? seq) seq)
               ((last-exp? seq) (first-exp seq))
               (else (make-begin seq))))
      ns)

(eval
	(do->let
		'(do ((i 1 (+ i 1))
			(sum 0 (+ sum i)))
			((> i 10) sum)
			(display i)(newline)
			))
	ns)

#|

(let ((i 1) (sum 0))
	(define (body)
		(sequence->exp
			(display i)
			((set! i (+ i 1))
			 (set! sum (+ sum i)))))
	(define (loop)
		(if (true? (> i 10))
			sum
			(sequence->exp
				((display i)
				 loop))))
	(sequence->exp
		((display i)
		 loop)))


(let ((i 1) (sum 0))
	(define (body)
		(sequence->exp
			((display i)
			 (set! i (+ i 1))
			 (set! sum (+ sum i)))))
	(define (loop)
		(if (> i 10)
			sum
			(sequence->exp
				((display i) (loop)))))
	(sequence->exp ((display i) (loop))))




(sequence->exp
	((display i) (set! i (+ i 1)) (set! sum (+ sum i))))

|#