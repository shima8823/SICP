(define (transfer from-account to-account amount)
	((from-account 'withdraw) amount)
	((to-account 'deposit) amount))

#|

Louis is correct
上のようなwithdrawとdepositだけを直列化するだけでは問題が生じる。
withdrawとdepositの間が直列化されてしないため、その間から新たなtransferプロセスが実行されてしまう可能性がある。
よって3_43.rktのexchangeプログラムのようにアカウントごとに直列化すべき。

answer
Louis is incorrect
譲渡のようなプログラムはexchangeのようなアカウントごとに直列化する必要はない。
from-accountのwithdrawが終了した後に別プロセスが実行されても何も問題はないから。

|#