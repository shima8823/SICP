; 良問

#lang racket

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
	(list left right
		(append (symbols left) (symbols right)) (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

; generic procedure
(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)))
(define (weight tree)
	(if (leaf? tree)
		(weight-leaf tree)
		(cadddr tree)))

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
						(let ((right (encode-iter (right-branch tree) (append bit '(1)))))
							(if (not (null? right))
								right
								(error "unknown symbol: " symbol))))
				)
			))
	)
	(encode-iter tree '())
)

; 別解
(define (encode-symbol symbol tree)
  (if (leaf? tree) '()
      (let ((lb (left-branch tree)) (rb (right-branch tree)))
        (cond ((element-of-set? symbol (symbols lb))
               (cons 0 (encode-symbol symbol lb)))
              ((element-of-set? symbol (symbols rb))
               (cons 1 (encode-symbol symbol rb)))))))

(define sample-tree
	(make-code-tree
		(make-leaf 'A 4)
		(make-code-tree
			(make-leaf 'B 2)
			(make-code-tree
				(make-leaf 'D 1)
				(make-leaf 'C 1)))))
(define sample-message '(A D A B B C A))

(define sample-encode (encode sample-message sample-tree))
sample-encode
(decode sample-encode sample-tree)
; (encode-symbol 'A sample-tree)
; (append '(0) '(10))