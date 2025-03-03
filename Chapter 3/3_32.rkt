#lang racket
(require r5rs) ; set-(car|cdr)!

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
	(if (empty-queue? queue)
		(error "FRONT called with an empty queue" queue)
		(car (front-ptr queue))))

(define (insert-queue! queue item)
	; (let ((new-pair (cons item '())))
	; 	(cond
	; 		((empty-queue? queue)
	; 			(set-front-ptr! queue new-pair)
	; 			(set-rear-ptr! queue new-pair)
	; 		queue)
	; 		(else
	; 			(set-cdr! (rear-ptr queue) new-pair)
	; 			(set-rear-ptr! queue new-pair)
	; 		queue)))
	(front-insert-queue! queue item)
)

(define (front-insert-queue! queue item)
	(let ((new-pair (cons item '())))
		(cond
			((empty-queue? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair)
			queue)
			(else
				(set-cdr! new-pair (front-ptr queue))
				(set-front-ptr! queue new-pair)
			queue))))

(define (delete-queue! queue)
	(cond
		((empty-queue? queue)
			(error "DELETE! called with an empty queue" queue))
		(else
			(set-front-ptr! queue (cdr (front-ptr queue)))
			queue)))


(define (inverter input output)
	(define (logical-not s)
		(cond
			((= s 0) 1)
			((= s 1) 0)
			(else (error "Invalid signal" s))))
	(define (invert-input)
		(let ((new-value (logical-not (get-signal input))))
			(after-delay
				inverter-delay
				(lambda () (set-signal! output new-value)))))
	(add-action! input invert-input)
	'ok)

(define (and-gate a1 a2 output)
	(define (logical-and a1 a2)
		(cond
			((= a1 0) 0)
			((= a2 0) 0)
			(else 1)))
	(define (and-action-procedure)
		(let ((new-value (logical-and (get-signal a1) (get-signal a2))))
			(after-delay
				and-gate-delay
				(lambda () (set-signal! output new-value)))))
	(add-action! a1 and-action-procedure)
	(add-action! a2 and-action-procedure)
	'ok)

(define (or-gate o1 o2 output)
	(define (logical-or o1 o2)
		(cond
			((= o1 1) 1)
			((= o2 1) 1)
			(else 0)))
	(define (or-action-procedure)
		(let ((new-value (logical-or (get-signal o1) (get-signal o2))))
			(after-delay
				or-gate-delay
				(lambda () (set-signal! output new-value)))))
	(add-action! o1 or-action-procedure)
	(add-action! o2 or-action-procedure)
	'ok)

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

(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
	(set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) 
	(set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))


(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


(define (add-to-agenda! time action agenda)
	(define (belongs-before? segments)
		(or (null? segments)
			(< time (segment-time (car segments)))))
	(define (make-new-time-segment time action)
		(let ((q (make-queue)))
			(insert-queue! q action)
			(make-time-segment time q)))
	(define (add-to-segments! segments)
		(if (= (segment-time (car segments)) time)
			(insert-queue! (segment-queue (car segments)) action)
			(let ((rest (cdr segments)))
				(if (belongs-before? rest)
					(set-cdr!
						segments
						(cons (make-new-time-segment time action) (cdr segments)))
					(add-to-segments! rest)))))

	(let ((segments (segments agenda)))
		(if (belongs-before? segments)
			(set-segments!
				agenda
				(cons (make-new-time-segment time action) segments))
			(add-to-segments! segments))))


(define (remove-first-agenda-item! agenda)
	(let ((q (segment-queue (first-segment agenda))))
		(delete-queue! q)
		(if (empty-queue? q)
			(set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
	(if (empty-agenda? agenda)
		(error "Agenda is empty: FIRST-AGENDA-ITEM")
	(let ((first-seg (first-segment agenda)))
		(set-current-time!
			agenda
			(segment-time first-seg))
		(front-queue (segment-queue first-seg)))))


(probe 'sum sum)
; sum 0 New-value = 0
(probe 'carry carry)
; carry 0 New-value = 0
(half-adder input-1 input-2 sum carry)
; ok
(set-signal! input-1 1)
; done

; queueのinsertをfront-incertにした時、1個ずつpropagateを実行すると
; たまたま上手く実行されてしまうため、comment out!
; (propagate)

; sum 8 New-value = 1 done
; -> or + and = s
(set-signal! input-2 1)
; done
(propagate)
; carry 11 New-value = 1
; -> 8 + and = 11
; sum 16 New-value = 0
; -> 8 + and + interver + and = 16
; done

#|

最初に予定表にれたものを実行しないと、wireの値がおかしくなってしまう
例えば2回目の(propagate)でintervalを呼んで(0になる)、
half-adderの(and-gate d e s)が発火してしまう。
そうすると(and-gate)の後に、それまでagendaに蓄えてきたイベントが発火することになる。
よってwireの値が正しくなくなる。

|#
