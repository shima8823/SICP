; 良問

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

(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
	(cond
		((null? pairs) '())
		((null? (cdr pairs)) (car pairs)) ; length=1?
		(else
			(successive-merge 
				(adjoin-set (make-code-tree (car pairs) (cadr pairs)) (cddr pairs))))
			; answer ソートするのを忘れていた
			; cons->adjoin-setにするべき
	)
)

; (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
(generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

#|

'(
	(leaf A 8)
	(
		(
			(
				(leaf H 1) (leaf G 1) (H G) 2
			)
			
			(
				(leaf F 1) (leaf E 1) (F E) 2
			)
			(H G F E) 
			4
		)
		(
			(
				(leaf D 1) (leaf C 1) (D C) 2
			) 
			(leaf B 3)
			(D C B) 
			5
		)
		(H G F E D C B)
		9
	)
	(A H G F E D C B)
	17
)

|#