#lang sicp

(define (require p) (if (not p) (amb)))
(define (an-element-of items)
	(require (not (null? items)))
	(amb (car items) (an-element-of (cdr items))))

(define (distinct? items)
	(cond ((null? items) true)
		  ((null? (cdr items)) true)
		  ((member (car items) (cdr items)) false)
		  (else (distinct? (cdr items)))))

(define (father-cruiser)
	(let ((M (amb 'D 'H 'B 'P))
		  (D (amb 'M 'H 'B 'P))
		  (H (amb 'M 'D 'B 'P))
		  (B (amb 'M 'D 'H 'P))
		  (P (amb 'M 'D 'H 'B)))
		(require (distinct? (list M D H B P)))
		(require (eq? D 'B)) ; D Bの娘
		(require (eq? P 'M)) ; P Mの娘
		(require
			(eq?
				(cond ((eq? B 'H) H)
			 		  ((eq? B 'M) M))
				'P))
		(list (list 'M M) (list 'D D)
			(list 'H H) (list 'B B)
			(list 'P P))))

(father-cruiser)

#|

M D H B P
  B     M
L M R G A

Gの⽗親は、Parker 博⼠の娘の名前をつけたクルーザーを持っている。
->１番目の例だとGの父親(H)はPの娘を持っていない。
父の苗字		M D H B P
娘フルネーム	P B D H M
				L M R G A

３番目もGの父親DはPの娘を持っていない。
よって2番目

			2			3
M D H B P	M D H B P	M D H B P
  B P   M	D B P H M	H B P D M
L M R G A	L M R G A	L M R G A

|#