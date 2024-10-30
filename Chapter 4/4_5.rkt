; return

#lang sicp

(define (assoc key records)
	(cond
		((null? records) false)
		((equal? key (caar records)) (car records))
		(else (assoc key (cdr records)))))

; (cond
; 	((assoc 'b '((a 1) (b 2))) => cadr)
; 	(else false))

(cond
	((equal? '(ab cb) '(ab cb)) => (lambda (x) (display "hello ") x))
	(else false))

(define (expand-clauses clauses)
	(if (null? clauses)
		'false ; else 節はない 最終的に再起される
		(let ((first (car clauses))
			  (rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error " ELSE clause isn't last : COND->IF " clauses))
				(make-if
					(cond-predicate first)
					(if (cond-recipient? (cond-actions first))
						(sequence->exp (list ((car (cond-actions (cdr first))) (cond-predicate first))))
						(sequence->exp (cond-actions first))
					)
					(expand-clauses rest))))))

(define (cond-recipient? exp) (eq? (first-exp exp) '=>))