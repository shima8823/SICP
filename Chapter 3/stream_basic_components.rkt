; Add below comment-code to file
; (#%require "./stream_basic_components.rkt")

#lang sicp
(#%require (only racket/base provide)) ; import provide from racket
(provide ; export
	cons-stream
	stream-car
	stream-cdr
	stream-ref
	stream-map
	stream-for-each
	stream-enumerate-interval
	display-stream
	display-line)

; define

(define-syntax cons-stream
	(syntax-rules ()
		((_ a b) (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (memo-proc proc)
	(let ((already-run? false) (result false))
		(lambda ()
			(if (not already-run?)
				(begin (set! result (proc))
					   (set! already-run? true)
						result)
				result))))

(define-syntax delay
	(syntax-rules ()
		; ((_ exp) (memo-proc (lambda () exp)))))
		((_ exp) (lambda () exp))))
(define (force delayed-obj)
	(delayed-obj))

(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map
				(cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
	(if (stream-null? s)
		'done
		(begin (proc (stream-car s))
			(stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream
			low
			(stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
	(stream-for-each display-line s))
(define (display-line x) (newline) (display x))