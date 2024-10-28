#lang sicp
(#%require "./stream_basic_components.rkt")

(define (integral delayed-integrand initial-value dt)
	(define int
		(cons-stream
			initial-value
			(let ((integrand (force delayed-integrand)))
				(add-streams (scale-stream integrand dt) int))))
	int)

(define (solve f y0 dt)
	(define y (integral (delay (dy)) y0 dt))
	(define (dy) (stream-map f y))
	y)

; 数学はわからん。
(define (solve-2nd a b dt y0 dy0)
	(define y (integral (delay (dy)) y0 dt))
	(define (dy) (integral (delay (ddy)) dy0 dt))
	(define (ddy)
		(add-streams
			(scale-stream (dy) a)
			(scale-stream y b)))
	y)

(display-stream-until
	10
	(solve-2nd
		1
		2
		0.1
		1
		0))

#|

ステップごとの数値

ステップ 時間𝑡 位置y 速度dy/dt​
0		0.0		1.0		0.0
1		0.1		1.0		-0.2
2		0.2		0.98		-0.396
3		0.3		0.9404		-0.58408
4		0.4		0.881992		-0.7606784
5		0.5		0.80592416		-0.922862592
6		0.6		0.7136379		-1.06793797
7		0.7		0.6068441		-1.19350679
8		0.8		0.4874934		-1.29739491
9		0.9		0.35775391		-1.37776669
10		1.0		0.22097724		-1.43310247

|#