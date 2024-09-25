; 良問

#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))


(define (list->tree elements)
	(car (partial-tree elements (length elements))))

(define (partial-tree elts n)
	(if (= n 0)
		(cons '() elts) ; leftとrightの場合で呼ばれる。
		(let ((left-size (quotient (- n 1) 2))) ; entry以外を2で割ってeltsの中の分けられる要素の数
			(let ((left-result (partial-tree elts left-size))) ;左枝とbalanceしてない組み合わせ
				(let
					((left-tree (car left-result)) ;左枝
					(non-left-elts (cdr left-result)) ;後のbalanceする前のelts
					(right-size (- n (+ left-size 1)))) ; (- (- n 1) left-size)の方がわかりやすよう
					(let
						((this-entry (car non-left-elts)) ; ここまでの左枝の親
						(right-result (partial-tree (cdr non-left-elts) right-size ))) ; 右枝とbalanceしてない組み合わせ
						(let
							((right-tree (car right-result)) ;右枝
							(remaining-elts (cdr right-result))) ;後のbalanceする前のelts
							(cons (make-tree this-entry left-tree right-tree) remaining-elts ))))))))

; '(1 3 5 7 9 11)
; '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

; (list->tree '(1 3 5 7 9 11))

#|

a
順序つきリストなので2で割った上限を超えないような最初の3つの値を取り左枝から右枝を作りtreeを作る。
コメント参照

	5
1		9
 3	7		11

b
'(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
歳小のtreeを作るのに2step必要で、それが6個あるので12stepかかる
よってO(n)

補足
treeは1回しかタッチしないため。

|#

(list->tree '(1 3 5))
