#lang sicp
(#%require "./stream_basic_components.rkt")

(define (make-zero-crossings input-stream last-value)
	(cons-stream
		(sign-change-detector
			(stream-car input-stream)
			last-value)
		(make-zero-crossings
			(stream-cdr input-stream)
			(stream-car input-stream))))

(define (make-zero-crossings input-stream last-avpt last-value)
	(let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
		(cons-stream
			(sign-change-detector
				avpt last-avpt)
			(make-zero-crossings
				(stream-cdr input-stream) avpt (stream-car input-stream)))))

#|

センサデータの値と直前の値との平均を取るが平均同士を比較することによって
正しくなりそう。


{make-zero-crossings
((1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4) 0)
0
((2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4) 0.5)
0
((1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4) 1.25)
0
((1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4) 0.875)
0
((0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4) 0.9375)
0
((-0.1 -2 -3 -2 -0.5 0.2 3 4) 0.71875)
0 !!
((-2 -3 -2 -0.5 0.2 3 4) 0.61875)
-1
((-3 -2 -0.5 0.2 3 4) -1.309375)
0
((-2 -0.5 0.2 3 4) -2.1546875)
0
((-0.5 0.2 3 4) -2.07734375)
0
((0.2 3 4) -1.288671875)
0
((3 4) -0.5443359375)
1
((4) -0.5443359375)
0
((...) 2.4556640625)

}
(make-zero-crossings (1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4) 0)


|#