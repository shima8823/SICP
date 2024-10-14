#lang racket
(require r5rs) ; set-(car|cdr)!


(define (make-table)
	(define (assoc key records)
		(cond
			((null? records) false)
			((equal? key (caar records)) (car records))
			(else (assoc key (cdr records)))))

	(let ((local-table (list '*table*)))

		(define (cons-keys keys value)
			(cond
				((= (length keys) 1) (cons (car keys) value))
				(else (cons (car keys) (cons-keys (cdr keys) value)))
			))

		;records = (cdr local-table) | subtable
		(define (assoc-apply keys value records); f-record-found f-record-not-found f-subtable-not-found
			(cond
				; ((null? records) false)
				((= (length keys) 1)
					(let ((record (assoc (car keys) (cdr records))))
						(if record
							(set-cdr! record value)
							(set-cdr! records (cons (cons (car keys) value) (cdr records))))))
				(else
					(let ((subtable
						(assoc (car keys) records))) ;2回目だったら cdrしたい
						(if subtable
							(assoc-apply (cdr keys) value subtable); f-record-found f-record-not-found f-subtable-not-found
							(set-cdr! local-table
								; (cons (list (car keys) (cons (cdr keys) value)) ;(cons (cadr keys) (cons (caddr keys) value))
								(cons (list (car keys) (cons-keys (cdr keys) value)) ;(cons (cadr keys) (cons (caddr keys) value))
									(cdr local-table)))
						)
					)
				)
			)
		)

		(define (lookup-assoc keys records)
			(cond
				((null? records) false)
				((= (length keys) 1)
					(let ((record (assoc (car keys) records)))
						(if record
							(cdr record)
							false)))
				(else
					(let ((subtable
						(assoc (car keys) records)))
						(if subtable
							(lookup-assoc (cdr keys) (cdr subtable)); f-record-found f-record-not-found f-subtable-not-found
							false
						)
					)
				)
			)
		)

		; (define (lookup key-1 key-2)
		(define (lookup keys)
			; (let ((subtable
			; 		(assoc key-1 (cdr local-table))))
			; 	(if subtable
			; 		(let ((record (assoc key-2 (cdr subtable))))
			; 			(if record
			; 				(cdr record)
			; 				false))
			; 		false))
			(lookup-assoc keys (cdr local-table))
		)

		(define (insert! keys value)
			; (let ((subtable
			; 		(assoc key-1 (cdr local-table))))
			; 	(if subtable
			; 		(let ((record
			; 			(assoc key-2 (cdr subtable))))
			; 			(if record
			; 				(set-cdr! record value)
			; 				(set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
			; 		(set-cdr! local-table
			; 			(cons (list key-1 (cons key-2 value))
			; 				(cdr local-table)))))
			(assoc-apply keys value (cdr local-table))
			'ok)

		(define (dispatch m)
			(cond ((eq? m 'lookup-proc) lookup)
				((eq? m 'insert-proc!) insert!)
				(else (error "Unknown operation: TABLE" m))))
	dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put '(math) 'complex)

(put '(math +) 43)
(put '(math -) 45)
(put '(math *) 42)

(get '(math +))
(get '(math -))
(get '(math *))

(put '(country asia test) '(japanese chinese korean etc))
; (put '(country asia japanese prefecture) '(Tokyo Kyoto Osaka etc))
; (put '(country asia china prefecture) '(Beijin Shanghai etc))
; (put '(country asia korea prefecture) '(Busan Pyonyan etc))

(get '(country asia))

