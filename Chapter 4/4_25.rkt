#lang sicp

(define (unless condition usual-value exceptional-value)
	(if condition exceptional-value usual-value))

(define (factorial n)
	(unless (= n 1)
			(* n (factorial (- n 1)))
			1))

#|

適用順序は無限ループ
正規順序言語は動く

|#

(factorial 5)