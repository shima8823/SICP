#lang sicp

#|

仕様は一つ目のフレームだけ削除される。
束縛を削除したいときはどういう時かというと、
現フレームでよりも後ろのフレームの束縛を使いたい時である。
なので一つ目のフレームだけ削除される仕様にした。

|#

(define (make-unbound! var env)
	(let ((frame (first-frame env)))
		(define (scan vars vals prev-vars prev-vals)
			(cond
				((null? vars)
					(error " Already Unbound variable : " var))
				((eq? var (car vars))
					(if (null? prev-vars)
						(begin (set-car! frame (cdr vars))
							(set-cdr! frame (cdr vals)))
						(begin (set-cdr! prev-vars (cdr vars))
							(set-cdr! prev-vals (cdr vals)))))
				(else (scan (cdr vars) (cdr vals) vars vals))))
		(scan (frame-variables frame) (frame-values frame)
				'() '())))