#lang sicp
(#%require "./stream_basic_components.rkt")

(define (average x y) (/ (+ x y) 2.0))
(define (square x) (* x x))

(define (sqrt-improve guess x)
	(average guess (/ x guess)))

; (define (sqrt-stream x)
; 	(define guesses
; 		(cons-stream
; 			1.0
; 			(stream-map
; 				(lambda (guess) (sqrt-improve guess x))
; 				guesses)))
; 	guesses)

(define (sqrt-stream x)
	(cons-stream
		1.0
		(stream-map
			(lambda (guess) (sqrt-improve guess x))
			(sqrt-stream x))))

#|

mapでguessesを更新した値を使っていないので同じ計算をしてしまう。
よって非効率である。

非効率
1.0
1.5 -> (improve 1.0)
1.4 -> (improve (improve 1.0))

memolize
1.0
1.5 -> (improve 1.0)
1.4 -> (improve 1.5)

memo-procを使用していなくても違いはある。上記例から。

補足
memo-procを使用していても新しく手続きを呼び出しているため((sqrt-stream x))
同じオブジェクトと認識されないはず。

|#

(display-stream-until 5 (sqrt-stream 2))