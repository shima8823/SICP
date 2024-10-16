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
			(ripple-carry-adder (cdr a-wires) (cdr b-wires) carry (cdr s-wires))
		))
)

(define (make-wire)
	(let ((signal-value 0) (action-procedures '()))
		(define (set-my-signal! new-value)
			(if (not (= signal-value new-value))
			(begin (set! signal-value new-value) (call-each action-procedures))
			'done))

		(define (accept-action-procedure! proc)
			(set! action-procedures
				(cons proc action-procedures))
			(proc))
		
		(define (dispatch m)
			(cond ((eq? m 'get-signal) signal-value)
				  ((eq? m 'set-signal!) set-my-signal!)
				  ((eq? m 'add-action!) accept-action-procedure!)
				  (else (error "Unknown operation: WIRE" m))))
	dispatch))

(define (call-each procedures)
	(if (null? procedures)
		'done
		(begin ((car procedures))
				(call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
	(add-to-agenda! (+ delay (current-time the-agenda))
					action
					the-agenda))


(define (propagate)
	(if (empty-agenda? the-agenda) 'done
		(let ((first-item (first-agenda-item the-agenda)))
			(first-item)
			(remove-first-agenda-item! the-agenda)
			(propagate))))

(define (probe name wire)
	(add-action!
		wire
		(lambda ()
			(newline)
			(display name)
			(display " ")
			(display (current-time the-agenda))
			(display " New-value = ") (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
; sum 0 New-value = 0
(probe 'carry carry)
; carry 0 New-value = 0
(half-adder input-1 input-2 sum carry)
; ok

#|
half-adderの各gateは
初期化時には
aciton-procedureにagendaに予定を追加する処理を追加し、
agendaに予定を追加している

set-signal!でagendaに予定(各ワイヤーの値を再度計算させる処理)を再追加

propagateでagendaにある処理を全て実行する

以下のようにした場合、値が計算されない

(define (accept-action-procedure! proc)
	(set! action-procedures
		(cons proc action-procedures)))

補足 from http://community.schemewiki.org/?sicp-ex-3.31
・初期化段階でこれらのアクションを手動でトリガーする必要があります。
・関数要素の定義では add-action を使用します。初期化しないと、この機能要素がシステムに挿入された時刻はアジェンダに記録されません。そのため、アジェンダが propagate によってシミュレートされたときに、関数要素がまったくシミュレートされない可能性があります。

|#

(set-signal! input-1 1)
; done
(propagate)
; sum 8 New-value = 1 done
(set-signal! input-2 1)
; done
(propagate)
; carry 11 New-value = 1
; sum 16 New-value = 0
; done

