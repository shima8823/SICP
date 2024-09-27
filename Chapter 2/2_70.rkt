#lang racket

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)))
(define (weight tree)
	(if (leaf? tree)
		(weight-leaf tree)
		(cadddr tree)))

(define (make-code-tree left right)
	(list left right
		(append (symbols left) (symbols right)) (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (adjoin-set x set)
	(cond ((null? set) (list x))
		  ((< (weight x) (weight (car set))) (cons x set))
		  (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
	(if (null? pairs)
		'()
		(let ((pair (car pairs)))
		  (adjoin-set (make-leaf (car pair)   ; symbol
								 (cadr pair)) ; frequency
					  (make-leaf-set (cdr pairs))))))

(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
			'()
			(let ((next-branch (choose-branch (car bits) current-branch)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
					(decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))

(define (choose-branch bit branch)
	(cond ((= bit 0) (left-branch branch))
		  ((= bit 1) (right-branch branch))
		  (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (encode message tree)
	(if (null? message)
		'()
		(append (encode-symbol (car message) tree)
				(encode (cdr message) tree))))

(define (encode-symbol symbol tree)
	(define (encode-iter tree bit)
		(cond ((null? tree) '())
			  ((leaf? tree)
			  	(if (eq? symbol (car (symbols tree)))
					bit
					'()))
			  (else 
				(let ((left (encode-iter (left-branch tree) (append bit '(0)))))
					(if (not (null? left))
						left
						; rightがなくてもエラーにしないことにした。
						(encode-iter (right-branch tree) (append bit '(1))))
				)
			))
	)
	(encode-iter tree '())
)

(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
	(cond
		((null? pairs) '())
		((null? (cdr pairs)) (car pairs)) ; length=1?
		(else
			(successive-merge 
				(adjoin-set (make-code-tree (car pairs) (cadr pairs)) (cddr pairs))))
	)
)

(define rock-1950 (generate-huffman-tree '((a 2) (Get 2) (Sha 3) (Wah 1) (boom 1) (job 2) (na 16) (yip 9))))
rock-1950
(encode '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom)
rock-1950
)
; '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0)

(length 
(encode '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom)
rock-1950
)
) ;符号化に84bit

; 8文字を表すのに3bitの固定長bitが必要
; 3 
(
	*
	(length '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
	3
)
; 108bit必要

; (
; 	(leaf na 16)
; 	(
; 		(leaf yip 9)
; 			(
; 				(
; 					(leaf a 2)
; 					(
; 						(leaf boom 1)
; 						(leaf Wah 1)
; 						(boom Wah)
; 						2
; 					)
; 					(a boom Wah)
; 					4
; 				)
; 				(
; 					(leaf Sha 3)
; 					(
; 						(leaf job 2)
; 						(leaf Get 2)
; 						(job Get)
; 						4
; 					)
; 					(Sha job Get)
; 					7
; 				) 
; 				(a boom Wah Sha job Get)
; 				11
; 			)
; 			(yip a boom Wah Sha job Get)
; 			20
; 		)
; 	(na yip a boom Wah Sha job Get)
; 	36
; )