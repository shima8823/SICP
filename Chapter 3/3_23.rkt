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
		(car (front-ptr queue))))

(define (rear-deque queue)
	(if (empty-deque? queue)
		(error "FRONT called with an empty queue" queue)
		(car (rear-ptr queue))))

(define (front-insert-deque! queue item)
	(let ((new-pair (cons item '())))
		(cond
			((empty-deque? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair)
			queue)
			(else
				(set-cdr! new-pair (front-ptr queue))
				(set-front-ptr! queue new-pair)
			queue))))

(define (rear-insert-deque! queue item)
	(let ((new-pair (cons item '())))
		(cond
			((empty-deque? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair)
			queue)
			(else
				(set-cdr! (rear-ptr queue) new-pair)
				(set-rear-ptr! queue new-pair)
			queue))))


(define (front-delete-deque! queue)
	(cond
		((empty-deque? queue)
			(error "DELETE! called with an empty queue" queue))
		(else
			(set-front-ptr! queue (cdr (front-ptr queue)))
			queue)))

(define (rear-delete-deque! queue)
	(cond
		((empty-deque? queue)
			(error "DELETE! called with an empty queue" queue))
		(else
			(set-rear-ptr! queue '(d))
			queue)))

(define (print-queue queue)
	(display (front-ptr queue))
	(newline))


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
; (front-deque q1)
; (rear-deque q1)

