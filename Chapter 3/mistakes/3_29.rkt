; 良問

#lang racket

(define (inverter input output)
	(define (invert-input)
		(let ((new-value (logical-not (get-signal input))))
			(after-delay
				inverter-delay
				(lambda () (set-signal! output new-value)))))
	(add-action! input invert-input)
	'ok)

(define (logical-not s)
	(cond
		((= s 0) 1)
		((= s 1) 0)
		(else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
	(define (and-action-procedure)
		(let ((new-value (logical-and (get-signal a1) (get-signal a2))))
			(after-delay
				and-gate-delay
				(lambda () (set-signal! output new-value)))))
	(add-action! a1 and-action-procedure)
	(add-action! a2 and-action-procedure)
	'ok)

(define (logical-and a1 a2)
	(cond
		((= a1 0) 0)
		((= a2 0) 0)
		(else 1)))


(define (or-gate o1 o2 output)
	(define (or-action-procedure)
		(let ((new-value (logical-or (get-signal o1) (get-signal o2))))
			(after-delay
				(+ (* inverter-delay 3) and-gate-delay)
				(lambda () (set-signal! output new-value)))))
	(add-action! o1 or-action-procedure)
	(add-action! o2 or-action-procedure)
	'ok)

(define (logical-or o1 o2)
	(logical-not
		(logical-and
			(logical-not o1)
			(logical-not o2))))

#|
and-gate-delay + inverter-delay *3
answer
and-gate-delay + inverter-delay *2
なぜなら電気信号はa1かa2が変えられた時しか走らない、
つまり1方が変更されたらもう1方はそのままの値でand-gateに入力される

inverterとand-gate自体を使う回答
https://sicp.iijlab.net/solution/ex3.3.html#ex3.25
>
(define (or-gate a1 a2 output)
	(let ((b1 (make-wire)) (b2 (make-wire)) (c (make-wire)))
		(inverter a1 b1)
		(inverter a2 b2)
		(and-gate b1 b2 c)
		(inverter c output)
 	'ok))

|#