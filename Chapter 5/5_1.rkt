(define (factorial n)
	(define (iter product counter)
		(if (> counter n)
			product
			(iter (* counter product)
				  (+ counter 1))))
	(iter 1 1))

#|

データパス図
p		c→>
		   |
		   n
	t<-(m p*c)
	p<-t
	t<-(a c+1)
	c<-t

コントローラ図

> no t<-m
	 p<-t
	 t<-a
	 c<-t

|#