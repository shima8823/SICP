#|

a = 10
b = 20
c = 30

exchangeは単に二つのアカウントのbalanceを逆にするだけなので
プロセスが逐次的に実行されるとしたらなんらかの順番で10, 20, 39ドルになる。

|#

(define (exchange account1 account2)
	(let ((difference (- (account1 'balance)
						(account2 'balance))))
		((account1 'withdraw) difference)
		((account2 'deposit) difference)))

#|
	Peter		B1		B2		Paul
|				10		20
|	(exc 1 2)
|	dif=-10
|	B1 wd -10	
|				20				(exc 1 2)
|								dif=0
|								B1 wd 0
|	B2 dp -10	20				B2 dp 0
|						10
|						20
|

|#

(define (serialized-exchange account1 account2)
	(let ((serializer1 (account1 'serializer))
			(serializer2 (account2 'serializer)))
		((serializer1 (serializer2 exchange))
			account1
			account2)))

#|

直列化は2つのアカウントを直列化するので
他プロセスがその間にbalanceを操作することはない。

	Peter		B1		B2		Paul
|				10		20
|	(exc 1 2)
|	dif=-10
|	B1 wd -10	
|				20				(dp 1 100)
|	B2 dp -10					
|						10
|				100
|


|#