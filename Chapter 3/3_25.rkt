#lang racket
(require r5rs) ; set-(car|cdr)!

#|

(math -> ((* . 42) -> ((+ . 43) -> ((- . 45))))
(math . complex)
これは上書きされる

コードを綺麗にするやる気がなくなった...

|#

(define (make-table)
	(let ((local-table (list '*table*)))
		(define (cons-keys keys value)
			(cond
				((= (length keys) 1) (cons (car keys) value))
				(else (cons (car keys) (cons-keys (cdr keys) value)))
			))

		;records = (cdr local-table) | subtable
		(define (assoc-apply keys value records prev)
			(define (assoc key records)
				(cond
					((null? records) false)
					((not (pair? (car records))) (assoc key (cdr records)))
					((equal? key (caar records)) (car records))
					(else (assoc key (cdr records)))))

			(let ((record-or-subtable (assoc (car keys) records)))
				(if (= (length keys) 1)
					(let ((record record-or-subtable))
						(if record
							(set-cdr! record value)
							(if (null? records)
								(set-cdr! local-table (cons (list (car keys) value) (cdr local-table)))
								(set-cdr! records (cons (cons (car keys) value) (if (null? records) records (cdr records)))))
						)
					)
					(let ((subtable record-or-subtable))
						(if subtable
							(assoc-apply (cdr keys) value (cdr subtable) subtable)
							(if (null? records)
								(set-cdr! local-table
									(cons (list (car keys) (cons-keys (cdr keys) value)) ;(cons (cadr keys) (cons (caddr keys) value))
										(cdr local-table)))
								(set-cdr! prev
									(cons (list (car keys) (cons-keys (cdr keys) value)) records))
							))))))

		(define (lookup-assoc keys records)
			(define (assoc key records)
				(cond
					((null? records) false)
					((not (pair? (car records))) (assoc key (cdr records)))
					((equal? key (caar records)) (car records))
					(else (assoc key (cdr records)))))

			(cond
				((null? records) false)
				((= (length keys) 1)
					(let ((record (assoc (car keys) records)))
						(if record ; (complex . ()) or ('+' . 42)
							(let ((cdr-record (cdr record)))
								(if (and (pair? cdr-record) (= (length cdr-record) 1))
									(car cdr-record)
									cdr-record
								))
							false)))
				(else
					(let ((subtable
						(assoc (car keys) records)))
						(if subtable
							(lookup-assoc (cdr keys) (cdr subtable))
							false
						)
					)
				)
			)
		)

		(define (lookup keys)
			(lookup-assoc keys (cdr local-table)))

		(define (insert! keys value)
			(assoc-apply keys value (cdr local-table) (cdr local-table))
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
(get '(math))

(put '(math +) 43)
(put '(math -) 45)
(put '(math *) 42)

(get '(math +))
(get '(math -))
(get '(math *))

(put '(country asia) '(japanese chinese korean etc))
(put '(country asia japanese prefecture) '(Tokyo Kyoto Osaka etc))
(put '(country asia china prefecture) '(Beijin Shanghai etc))
(put '(country asia korea prefecture) '(Busan Pyonyan etc))

(display (get '(country asia)))(newline)
(display (get '(country asia japanese prefecture)))(newline)
(display (get '(country asia china prefecture)))(newline)
(display (get '(country asia korea prefecture)))(newline)
