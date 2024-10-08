#lang racket

(define (make-account balance password)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
					balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance)
	(define (correct-password? try)
		(define (find-passwords list)
			(cond ((null? list) #f)
				  ((eq? try (car list)) #t)
				  (else (find-passwords (cdr list)))))
		(if (pair? password)
			(find-passwords password)
			(eq? try password)))
	(define (add-password try)
		(let ((current-password (car try))
			  (new-password (cadr try)))
			(if (correct-password? current-password)
				(set! password
					(if (pair? password)
						(cons new-password password)
						(cons new-password (list password))))
				(error "You can not have permisson!"))))
	
	(define (dispatch try m)
		(cond
			((eq? m 'add-password) add-password)
			(else 
				(if (correct-password? try)
					(cond ((eq? m 'withdraw) withdraw)
						((eq? m 'deposit) deposit)
						((eq? m 'correct-password?) #t)
						((eq? m 'add-password) add-password)
						(else (error "Unknown request: MAKE-ACCOUNT" m)))
					(error "Incorrect password")))))
	dispatch)

(define (make-joint account password new-password)
	(begin
		((account password 'add-password) (list password new-password))
		account))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
	(make-joint peter-acc 'open-sesame 'rosebud))

paul-acc
((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'rosebud 'withdraw) 0)
((paul-acc 'rosebud 'withdraw) 50)
((peter-acc 'open-sesame 'withdraw) 0)