#|

正規順序評価ではpを展開しないで0を返す。
適用順序評価はtest手続きの呼び出しでpを展開しようとするため、値を返さない

|#

#lang lazy

(define (p) (p))
(define (test x y)
(if (= x 0) 0 y))

(test 0 (p))