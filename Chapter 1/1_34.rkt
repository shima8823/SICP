#|

実行できない
(f f)
(f 2) 期待されているのは(f g(関数))だから。

|#

#lang racket

(define (f g) (g 2))
(f f)