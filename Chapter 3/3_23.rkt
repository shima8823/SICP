; 良問

#lang racket
(require r5rs) ; set-(car|cdr)!

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-deque? queue) (null? (front-ptr queue)))
(define (make-deque) (cons '() '()))

(define (front-deque queue)
	(if (empty-deque? queue)
		(error "FRONT called with an empty queue" queue)
		(caar (front-ptr queue))))

(define (rear-deque queue)
	(if (empty-deque? queue)
		(error "FRONT called with an empty queue" queue)
		(caar (rear-ptr queue))))

(define (prev-pair pair) (cdar pair))

(define (front-insert-deque! queue item)
	(let ((new-pair (cons (cons item (cons '() '())) '()))) ; (b (() ()))
		(cond
			((empty-deque? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair)
			queue)
			(else
				(set-car! (prev-pair (front-ptr queue)) new-pair)
				; (display "new-pair: ")(display new-pair)(newline)
				; (display "car: ")(display (car new-pair))(newline)
				(set-cdr! new-pair (front-ptr queue))
				(set-front-ptr! queue new-pair)
			queue))))

(define (rear-insert-deque! queue item)
	(let ((new-pair (cons (cons item (cons '() '())) '())))
		(cond
			((empty-deque? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair)
			queue)
			(else
				(set-car! (cdar new-pair) (rear-ptr queue))
				(set-cdr! (rear-ptr queue) new-pair)
				(set-rear-ptr! queue new-pair)
			queue))))

; TODO null
(define (front-delete-deque! queue)
	(cond
		((empty-deque? queue)
			(error "DELETE! called with an empty queue" queue))
		(else
			(set-front-ptr! queue (cdr (front-ptr queue)))
			(set-car! (prev-pair (front-ptr queue)) '())
			queue)))

; TODO null
(define (rear-delete-deque! queue)
	(cond
		((empty-deque? queue)
			(error "DELETE! called with an empty queue" queue))
		(else ;空の場合
			; (display "delete: ")(display (rear-ptr queue))(newline)
			(set-rear-ptr! queue (cadar (rear-ptr queue)))
			(set-cdr! (rear-ptr queue) '())
			; (display "queue: ")(display queue)(newline)
			queue)))

(define (print-queue queue)
	; queue
	; (display queue)
	; (front-ptr queue)
	; (display (front-ptr queue)) ; ((a ()))
	(display (map car (front-ptr queue)))
	(newline)

	)


(define q1 (make-deque))

(print-queue (front-insert-deque! q1 'a))	; ((a) a)
(print-queue (front-insert-deque! q1 'b))	; ((b) a)
(front-deque q1)
(rear-deque q1)
(print-queue (rear-insert-deque! q1 'c))	; ((b a c) c)
(front-deque q1)
(rear-deque q1)

(print-queue (front-delete-deque! q1))	; ((a c) c)
(front-deque q1)
(rear-deque q1)
(print-queue (rear-delete-deque! q1))	; ((a) a)
(front-deque q1)
(rear-deque q1)

; (newline)
; (cons 'item (cons '() '()))
; (cons (cons 'item (cons '() '())) '())
; (display (cons 'item (cons '() '())))
; (newline)
; (display (cons (cons 'item (cons '() '())) '()))

; https://sicp.iijlab.net/solution/ex3.3.html
; #0=((b . #2=(#0#)) . #1=((a #1# . #2#)))
