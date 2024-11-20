#lang sicp

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
	(accumulate append '() (map proc seq)))

; (map (lambda (x) (display x) x) '(1 2 3 4 5))

(define (enumerate-interval low high)
	(if (> low high)
		'()
		(cons low (enumerate-interval (+ low 1) high))))

; (enumerate-interval 10 20)

; flatmapはseqのリストの値のリストを1階層削除してあげる。
; flatの意味は1階層値をあげる。mapはmap。
(flatmap (lambda (x) (display x)(newline) x) '((1 (20)) (2) (3) (4) (5)))

