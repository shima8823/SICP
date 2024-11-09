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
	(let ((D (amb 'M 'H 'B 'P)))
		(require (eq? D 'B)) ; D Bの娘
		(let ((P (amb 'M 'D 'H 'B)))
			(require (not (eq? P D)))
			; Comment out below if Mary Ann isn't known Moore
			(require (eq? P 'M)) ; P Mの娘
			(let ((M (amb 'D 'H 'B 'P)))
				(require (not (eq? M D)))
				(require (not (eq? M P)))
				(let ((H (amb 'M 'D 'B 'P)))
					(require (not (eq? H D)))
					(require (not (eq? H P)))
					(require (not (eq? H M)))
					(let ((B (amb 'M 'D 'H 'P)))
						(require (not (eq? B D)))
						(require (not (eq? B P)))
						(require (not (eq? B M)))
						(require (not (eq? B H)))
						(require
							(eq?
								(cond ((eq? B 'H) H) ((eq? B 'M) M))
								'P))
						(list (list 'M M) (list 'D D)
							(list 'H H) (list 'B B)
							(list 'P P))))
						))))

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

Mary Annの性がmooreと知らない場合 2解

M D H B P
  B      
L M R G A

1
M D H B P
D B P H M
L M R G A

2
M D H B P
P B D M H
L M R G A

|#