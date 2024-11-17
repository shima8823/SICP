(sum ?amount (and (job ?x (computer programmer))
				  (salary ?x ?amount)))

(rule sum)
	(accumulation-function ?amount (and (job ?x (computer programmer))
										(salary ?x ?amount)))

#|

集積できないと思う。上記のようにしたらaccumulation-functionが複数できてしまう。
(accumulation-function query)
(accumulation-function query)
(accumulation-function query)

打開策
list-valueでそれぞれのストリームを足していく。

↑は実装の詳細
; answer

例えば以下のようにしてしまうと、warbucksの給料が4回計算されてしまう。
(and (job (wheel ?who) (computer programmer))
	 (salary (wheel ?who) ?amount))

打開策
名前で重複をfilterする。

|#