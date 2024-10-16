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
				or-gate-delay
				(lambda () (set-signal! output new-value)))))
	(add-action! o1 or-action-procedure)
	(add-action! o2 or-action-procedure)
	'ok)

(define (logical-or o1 o2)
	(cond
		((= o1 1) 1)
		((= o2 1) 1)
		(else 0)))

(define (half-adder a b s c)
	(let ((d (make-wire)) (e (make-wire)))
		(or-gate a b d)
		(and-gate a b c)
		(inverter c e)
		(and-gate d e s) 'ok))

(define (full-adder a b c-in sum c-out)
	(let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
		(half-adder b c-in s c1)
		(half-adder a s sum c2)
		(or-gate c1 c2 c-out) 'ok))

(define (ripple-carry-adder a-wires b-wires s-wires c)
	(if (null? a-wires) 'ok
		(let ((carry (make-wire)))
			(full-adder (car a-wires) (car b-wires) c (car s-wires) carry)
			(ripple-carry (cdr a-wires) (cdr b-wires) carry (cdr s-wires))
		))
)

#|

time
	half-adder = or (* and 2) inventer
	full-adder = (if only a) (+ 1half-adder or)
				 (else) (+ (* 2 half-adder) or)
	(* n (+ (* 2 half-adder) or))

answer : https://sicp.iijlab.net/solution/ex3.3.html#ex3.30
時間の考察
full adder1段の時間の最大値は
sumはhalf adderのsumの時間の2倍
carryはhalf adderのsumの時間 + half adderのcarryの時間 + orの時間
n段のときは(carry時間 × (n - 1))+ (max (carry時間, sum時間)

carryとsumはcarry(n), sum(n)と考えるといい？
そうすれば足し算なのが納得できる

|#