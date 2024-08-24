; a-plus-abs-b演算子はbが正の値であればa+bになり、負の値であればa-bになる。
; よってbの絶対値をaに加算する式である。

(define (a-plus-abs-b a b)
	((if (> b 0) + -) a b))
