#lang racket

(define (cont-frac n d k)
	(define (iter i result)
		(if (= i k)
			result
			(iter (+ i 1) (/ (n i) (+ (d i) result)))
		)
	)
	(iter 1 (/ (n 1) (d 1)))
)
#|

(iter 1 (/ (n 1) (d 1)))
(iter 1 (/ 1 1))
(iter 2 (/ 1 2))
(/ 1 2)

(iter 1 1)
(iter 2 (/ (-1) 4))
(/ (-1) 4)

この式は固定された時だけ正しい値になるのに対して、
nが変動するような値の場合はそもそも別の式になってしまう。
n = kから計算していく「下から上へ」の計算にしないといけない。

|#


; (define (cont-frac n d k)
;   (define (cf res i)
;     (if (= i 0)
;         res
;         (cf (/ (n i) (+ (d i) res)) (- i 1))))
;   (cf (/ (n k) (d k)) (- k 1)))

#|

(cf (/ (n k) (d k)) (- k 1)))
(cf (/ -1 3) 1) 
(cf (/ 1 (+ 1 (/ -1 3)) 0)
(/ 1 (/ 2 3))

|#

(define (tan-cf x k)
	(cont-frac
		(lambda (i) 
			(if (= i 1)
				x
				(- (* x x))
			)
		)
		(lambda (i)
			(- (* 2 i) 1)
		)
		k
	)
)

(tan-cf 1 2)
; (tan-cf (/ 3.14159 4) 10)
