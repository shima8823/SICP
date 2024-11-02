; 良問 チューリングの有名な停⽌性問題

#lang sicp

(define (run-forever) (run-forever))
(define (try p)
	(if (halts? p p) (run-forever) 'halted))

#|
無限に動き続けることを検知することは無限に実行することしかできないので実装できない。

pが停止するならrun-forever, 停止しないならhalted

(try try)は矛盾した結果になる。
pが停止するならrun-forever, 停止しないならhalted

tryを停止すると判定したのに無限ループ、その逆も然り

よって仮定がおかしいはず。

|#
