; 良問 解釈ができなかった

#lang sicp
(#%require "../stream_basic_components.rkt")

(define (expand num den radix)
	(cons-stream
		(quotient (* num radix) den)
		(expand (remainder (* num radix) den) den radix)))

(expand 1 7 10)
#|

(expand 1 7 10)
(expand 3 7 10)
(expand 2 7 10)
(expand 6 7 10)
(expand 4 7 10)
(expand 5 7 10)
(expand 1 7 10)

'(1 4 2 8 5 7 loop)

|#
(stream-ref (expand 1 7 10) 6)
(expand 3 8 10)
#|

(expand 3 8 10)
(expand 6 8 10)
(expand 4 8 10)
(expand 0 8 10)

'(3 7 5 0 0 0 0 ...)

|#

(stream-ref (expand 3 8 10) 2)

#|

基数を基数とする (/ num den) の浮動小数点表現
(/ 1.0 7)
.14285714285714285 

(/ 3.0 8)
.375

|#