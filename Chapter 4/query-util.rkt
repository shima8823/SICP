#lang sicp

(#%require (only racket/base make-base-namespace))
(#%require (only racket/base provide))
(#%require (only racket/base all-defined-out))
(provide (all-defined-out))	; Export

; generic table

(define (make-table)
	(let ((local-table (list '*table*)))
		(define (lookup key-1 key-2)
			(let ((subtable (assoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (assoc key-2 (cdr subtable))))
						(if record (cdr record) false))
					false)))
		(define (insert! key-1 key-2 value)
			(let ((subtable (assoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (assoc key-2 (cdr subtable))))
						(if record
							(set-cdr! record value)
							(set-cdr!
								subtable
								(cons (cons key-2 value) (cdr subtable)))))
					(set-cdr!
						local-table
						(cons (list key-1 (cons key-2 value)) (cdr local-table)))))
			'ok)
		(define (dispatch m)
			(cond ((eq? m 'lookup-proc) lookup)
				  ((eq? m 'insert-proc!) insert!)
				  (else (error " Unknown operation : TABLE " m))))
	dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; ###


; stream

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

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

(define (stream-append s1 s2)
	(if (stream-null? s1)
		s2
		(cons-stream
			(stream-car s1)
			(stream-append (stream-cdr s1) s2))))

(define (display-stream s)
	(stream-for-each display-line s))

; ###


; other

(define user-initial-environment make-base-namespace)

(define (display-line x) (newline) (display x))
(define (prompt-for-input string)
	(newline) (newline) (display string) (newline))
(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false))

; ###
