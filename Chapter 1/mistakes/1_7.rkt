#|

例えば√2では少数部4桁目が違っている
√100では有効数字が決められていないため、少数部の後半に0以外の数字が出ている

もう一つの戦略についてはうまくいかない。
有効数字の問題が解決できてないのと効率性の問題で変わらないから。

answer
(sqrt 2) ==> 1.4142156862745097

(sqrt 0.02) ==> .1444238094866232

(sqrt 0.0002) ==> .03335281609280434

また大きい方は

(sqrt 200000000000000000000) good-enough?が真にならず停止しなくなる. 

|#

#lang racket
(define (sqrt x)
  (define (improve guess)
	(define (average x y)
	  (/ (+ x y) 2))
	(average guess (/ x guess)))
  (define (sqrt-iter last-guess next-guess)
	(define (good-enough?)
	  (< (abs (/ (- last-guess next-guess) next-guess))
		 0.001))
	(if (good-enough?)
		next-guess
		(sqrt-iter next-guess (improve next-guess))))
  (sqrt-iter 1.0 (improve 1.0)))

(sqrt 2)
(sqrt 200000000000000000000)