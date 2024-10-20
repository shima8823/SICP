#lang racket
(require r5rs) ; set-(car|cdr)!

(define unique 0)

; mutexする必要あり。
(define (get-unique)
	(set! unique (+ unique 1))
	unique)

(define (make-account balance)
	(define id (get-unique))

	(define (get-id) id)
	(define (withdraw amount)
		(if (>= balance amount)
		(begin (set! balance (- balance amount)) balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount)) balance)

	(let ((balance-serializer (make-serializer)))
		(define (dispatch m)
			(cond
				((eq? m 'withdraw) withdraw)
				((eq? m 'deposit) deposit)
				((eq? m 'balance) balance)
				((eq? m 'serializer) balance-serializer)
				((eq? m 'get-id) get-id)
				(else (error "Unknown request : MAKE-ACCOUNT " m))))
		dispatch))

(define (serialized-exchange account1 account2)
	(let ((serializer1 (account1 'serializer))
			(serializer2 (account2 'serializer)))
		(if (< (get-id account1) (get-id account2))
			((serializer2 (serializer1 exchange))
				account1
				account2)
			((serializer1 (serializer2 exchange))
				account1
				account2))))

#|

同じ口座をexchangeするときにデッドロックする。

|#