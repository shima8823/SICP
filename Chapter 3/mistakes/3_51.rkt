#lang racket
(require (prefix-in strm: racket/stream))

(define (list-ref items n)
	(if (= n 0)
		(car items)
		(list-ref (cdr items) (- n 1))))

(define (enumerate-interval low high)
	(if (> low high)
		'()
		(cons low (enumerate-interval (+ low 1) high))
	)
)

(define-syntax cons-stream
  (syntax-rules ()
	((_ a b) (strm:stream-cons a b))))
(define stream-car strm:stream-first)
(define stream-cdr strm:stream-rest)
(define stream-null? strm:stream-empty?)
(define the-empty-stream strm:empty-stream)

(define (stream-map proc s)
	(if (stream-null? s)
		the-empty-stream
		(cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))

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

(define (show x)
	(display-line x)
	x)

(define x
	(stream-map
		show
		(stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

#|

指定数字以外はdisplayされない

|#

; (define x-list
; 	(map
; 		show
; 		(enumerate-interval 0 10)))

; (list-ref x-list 5)
; (list-ref x-list 7)
