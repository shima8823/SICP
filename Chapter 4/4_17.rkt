#lang sicp

#|

変更前
env1 = frame
(define env1)
(lambda
	(define env1))

変更後
env1 = frame
env2 = (env1 frame)
(define env1)
(lambda
	(define env2))


環境構造のこの違いが正しいプログラムのふるまいには決して影響を及ぼさない理由
lambda内部の定義は内部でしか使わない && 外部で定義されていたら再定義になるのでletを使い1フレームを追加必要がある。
もし外部で定義されていた場合、lambda内部に定義することによって外部の定義を外部から呼び出すこと(外部のフレームからrootフレームに探査)ができるため。

"同時”スコープルールはself-applyのcompound-procedureを実行する際に拡張せずにenvを仕様すればいい。
補足 http://community.schemewiki.org/?sicp-ex-4.17
letを削除してdefineを全て上に持ってくる。

|#

(define (self-apply procedure arguments)
	(cond
		((primitive-procedure? procedure)
			(apply-primitive-procedure procedure arguments))
		((compound-procedure? procedure)
			(eval-sequence
				(procedure-body procedure)
				(procedure-environment procedure)))
		(else
			(error "Unknown procedure type : APPLY " procedure))))
