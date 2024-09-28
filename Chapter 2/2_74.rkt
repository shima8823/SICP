; 良問

#lang racket

#|

A事務所
(name: ((address: X) (salary: M)))

B事務所
(name: ((salary: N) (address: Y)))


|#

(define (type-tag datum)
	(if (pair? datum)
		(car datum)
		(error "Bad tagged datum: TYPE-TAG" datum)))

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args))
				(error 
					"No method for these types: APPLY-GENERIC"
					(list op type-tags))))))

; 各事業所にget-〇〇を実装してもらう
(define (office-name file) (car file))
(define (hr-records file) (cdr files))

; a: 事業所のファイルはファイル(リスト)の先頭に事業所の名前タグをつけてもらう
(define (get-record name file)
	((get 'get-record (office-name file))
				name (hr-records file)))

; b: 個人のレコードを受け取って事務所の構造から
; salaryのレコードを取得する
(define (get-salary record)
	((get 'get-salary (office-name record))
				(hr-records record)))

; c: 
(define (find-employee-record name all-files)
	(if (null? all-files)
		'()
		(let 
			(employee 
				((get 'get-record (office-name (car all-files)))
					(hr-records file))
			)
			(if (null? employee)
				(find-employee-record name (cdr all-files))
				employee)
		)
	)
)

; d:事業所のget-record, salary関数を本部のテーブルにマッピングする。
