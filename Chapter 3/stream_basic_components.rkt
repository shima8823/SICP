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
	stream-filter
	stream-enumerate-interval
	display-stream
	display-line
	display-stream-until

	ones
	integers
	add-streams
	sub-streams
	mul-streams
	div-streams

	scale-stream

	merge
	)

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

(define (stream-filter pred stream)
	(cond ((stream-null? stream) the-empty-stream)
		  ((pred (stream-car stream))
			(cons-stream
				(stream-car stream)
				(stream-filter
					pred
					(stream-cdr stream))))
		  (else (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream
			low
			(stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
	(stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (display-stream-until n stream)
	(define (iter i)
		(display (stream-ref stream i))(newline)
		(if (= n i)
			'done
			(iter (+ i 1))))
	(display (iter 0)) (newline) ; done\n
	(newline)
)

(define ones (cons-stream 1 ones))
(define integers
	(cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (sub-streams s1 s2) (stream-map - s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define (div-streams s1 s2) (stream-map / s1 s2))

(define (scale-stream stream factor)
	(stream-map
		(lambda (x) (* x factor))
		stream))

(define (merge s1 s2)
	(cond ((stream-null? s1) s2)
		  ((stream-null? s2) s1)
		  (else
			(let ((s1car (stream-car s1))
				  (s2car (stream-car s2)))
				(cond
					((< s1car s2car)
						(cons-stream
							s1car
							(merge (stream-cdr s1) s2)))
					((> s1car s2car)
						(cons-stream
							s2car
							(merge s1 (stream-cdr s2))))
					(else
						(cons-stream
							s1car
							(merge (stream-cdr s1) (stream-cdr s2)))))))))