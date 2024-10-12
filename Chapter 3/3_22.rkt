#lang racket
(require r5rs) ; set-(car|cdr)!

(define (make-queue)
	(define queue (cons '() '()))
	(let
		((front-ptr (lambda (queue) (car queue))) (rear-ptr (lambda (queue) (cdr queue))))
		(define (set-front-ptr! item) (set-car! queue item))
		(define (set-rear-ptr! item) (set-cdr! queue item))
		(define (empty-queue?) (null? (front-ptr queue)))
		(define (front-queue)
			(if (empty-queue?)
				(error "FRONT called with an empty queue" queue)
				(car (front-ptr queue))))
		(define (insert-queue! item)
			(let ((new-pair (cons item '())))
				(cond
					((empty-queue?)
						(set-front-ptr! new-pair)
						(set-rear-ptr! new-pair)
					queue)
					(else
						(set-cdr! (rear-ptr queue) new-pair)
						(set-rear-ptr! new-pair)
					queue))))
		(define (delete-queue!)
			(cond
				((empty-queue?)
					(error "DELETE! called with an empty queue" queue))
				(else
					(set-front-ptr! (cdr (front-ptr queue)))
					queue)))
		(define (print-queue)
			(display (front-ptr queue))
			(newline))
		(define (dispatch m)
			(cond
				((eq? m 'empty-queue?) empty-queue?)
				((eq? m 'front-queue) front-queue)
				((eq? m 'insert-queue!) insert-queue!)
				((eq? m 'delete-queue!) delete-queue!)
				((eq? m 'print-queue) print-queue)
				(else (error "Unknown instruction: " m))
			))
	dispatch))



(define q1 (make-queue))
((q1 'insert-queue!) 'a)
((q1 'print-queue))
((q1 'insert-queue!) 'b)
((q1 'print-queue))
((q1 'delete-queue!))
((q1 'print-queue))
((q1 'delete-queue!))
((q1 'print-queue))
; (print-queue (insert-queue! q1 'a))	; ((a) a)

; (print-queue (insert-queue! q1 'a))	; ((a) a)
; (print-queue (insert-queue! q1 'b))	; ((a b) b)
; (print-queue (delete-queue! q1))	; ((b) b)
; (print-queue (delete-queue! q1))	; (() b)
