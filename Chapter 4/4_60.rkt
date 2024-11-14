(rule (lives-near ?person-1 ?person-2)
	(and (address ?person-1 (?town . ?rest-1))
		 (address ?person-2 (?town . ?rest-2))
		 (not (same ?person-1 ?person-2))))

#|

なぜoccurか？
person-1, person-2のどちらともAlyssa, Cyとして値を取ることができるから。

アルファベット順でペアにする条件を追加する方法がある。

|#