#lang racket

(define (make-account-and-serializer balance)
	(define (withdraw amount)
		(if (>= balance amount)
		(begin (set! balance (- balance amount)) balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount)) balance)

	(let ((balance-serializer (make-serializer)))
		(define (dispatch m)
			(cond
				((eq? m 'withdraw) (balance-serializer withdraw))
				((eq? m 'deposit) (balance-serializer deposit))
				((eq? m 'balance) balance)
				((eq? m 'serializer) balance-serializer)
				(else (error "Unknown request : MAKE-ACCOUNT " m))))
		dispatch))

(define (serialized-exchange account1 account2)
	(let ((serializer1 (account1 'serializer))
			(serializer2 (account2 'serializer)))
		((serializer1 (serializer2 exchange))
			account1
			account2)))
; (define (t)
; 		(define (dispatch m)
; 			(cond
; 				((eq? m 'withdraw) (- (+ 5 3) 4))
; 				(else (error "Unknown request : t " m))))
; 		dispatch)

; (define tr (t))
; (tr 'withdraw)

#|

よくわからない。イメージとしてはデッドロックなんだろうけど
なぜ動的なシリアライザーだと上手く動くのかわからない。
シリアライザーの実装を見ないといけないようだ。
https://github.com/jiacai2050/sicp/blob/master/exercises/03/3.45.md
https://tmurata.hatenadiary.org/entry/20100209/1265672305

n時間後

以下は関係ないんだ
こいつが前提だと勘違いをしていたようだ。
(define (deposit account amount)
	(let ((s (account 'serializer))
		(d (account 'deposit)))
	((s d) amount)))

以下のようになっていてそれからserialized-exchangeするというだけの話
((eq? m 'withdraw) withdraw)
((eq? m 'deposit) deposit)
((eq? m 'balance) balance)

だから当然デッドロックが起こる。

|#