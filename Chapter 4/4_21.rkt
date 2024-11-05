; 良問 Yコンビネータ = 純粋ラムダ計算 = 無名再帰？
#lang sicp

((lambda (n)
	((lambda (fact) (fact fact n)) ; fact = (ft k)
	 (lambda (ft k)
	 	(if (= k 1)
			1
			(* k (ft ft (- k 1)))))))
	10)

#|

(
(lambda
	((lambda (ft k) ;自分自身を引数として渡している。
	 	(if (= k 1)
			1
			(* k (ft ft (- k 1)))))

	10)
	(if (= k 1)
		1
		(* k (ft ft (- k 1)))))
)

|#


#|

b.やりたいこと
(define (f x)
	(letrec
		((even? (lambda (n)	(if (= n 0) true (odd? (- n 1)))))
		 (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
	(even? x)))

|#

(define (f x)
	((lambda (even? odd?) (even? even? odd? x))
	 (lambda (ev? od? n)
		(if (= n 0) true (od? ev? od? (- n 1))))
	 (lambda (ev? od? n)
		(if (= n 0) false (ev? ev? od? (- n 1))))))

#|

(lambda
	((lambda (ev? od? n)
		(if (= n 0) true (od? ⟨??⟩ ⟨??⟩ ⟨??⟩))) ;even?
	(lambda (ev? od? n)
		(if (= n 0) false (ev? ⟨??⟩ ⟨??⟩ ⟨??⟩)))) ;odd?
	(even? even? odd? x))

|#

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)
(f 8)
(f 9)
(f 10)