; skip

#lang sicp
(#%require "../stream_basic_components.rkt")

(define (interleave s1 s2)
	(if (stream-null? s1)
		s2
		(cons-stream
			(stream-car s1)
			(interleave s2 (stream-cdr s1)))))

(define (pairs s t)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(interleave
			(stream-map
				(lambda (x) (list (stream-car s) x))
				(stream-cdr t))
			(pairs (stream-cdr s) (stream-cdr t)))))

#|

(1 1) (1 2) (1 3) (1 4) (1 5) 
	  (2 2) (2 3) (2 4) (2 5) 
			(3 3) (3 4) (3 5) 
				  (4 4) (4 5) 
						(5 5) 

下の木構造を見た方がわかりやすい
{
	(pairs (1 2 3 4 5...) (1 2 3 4 5...))
	(1 1)
	(interleave
		((1 2) (1 3) (1 4) (1 5)...)
		(pairs (2 3 4 5...) (2 3 4 5...)))
	[cons-stream
		(1 2)
	p	(interleave
			(pairs (2 3 4 5...) (2 3 4 5...)))
			((1 3) (1 4) (1 5)...)
	{pair
		(2 2)
		(interleave
			((2 3) (2 4) (2 5)...))
			(pairs (3 4 5...) (3 4 5...))
		)
		interleave = (2 3) (interleave (pairs (3 4 5... ) ((2 4)...))
	}
	p	; (interleave
		; 	((1 3) (1 4) (1 5)...))
		; 	(pairs (3 4 5...) (3 4 5...))
		; )
		[cons-stream
			(1 3)
			[cons-stream
	p			; (interleave
				; 	(pairs (3 4 5...) (3 4 5...))
				; 	((1 4) (1 5)...))
				; )
	{pair
		(3 3)
		(interleave
			((3 3) (3 4) (3 5)...))
			(pairs (4 5...) (4 5...))
		)
		interleave = (2 3) (interleave (pairs (3 4 5... ) ((2 4)...))
	}
		]
			[cons-stream
	p		; (interleave
			; 	((1 4) (1 5)...))
			; 	(pairs (4 5...) (4 5...))
			; )
				(1 4)
				; (interleave
				; 	(pairs (4 5...) (4 5...))
				; 	((1 5)...))
				; )
			]
			
		]
	]
			(interleave
				((2 3) (2 4) (2 5)...))
				(pairs (3 4 5...) (3 4 5...))
			(2 3)
			(1 4)
				(interleave
					(pairs (3 4 5...) (3 4 5...))
					((2 3) (2 4) (2 5)...))
				(3 3)
}


              P(1 2 3 4 5) (1 2 3 4 5)
                           |
                         (1 1)
                           |
                         (1 2)
                        /       \
          P(2 3 4 5) (2 3 4 5)   (1 3)
                      |               \
                     (2 2)             (1 4)
                    /      \               \
                 (2 3)    P(3 4 5) (3 4 5)  (1 5)
                  /         |
              (2 4)       (3 3)
               /          /     \
            (2 5)       (3 4)   P(4 5) (4 5)
                        /              |
                      (3 5)         (4 4)
                                     |
                                   (4 5)
                                     |
                                 P(5) (5)
                                       |
                                     (5 5)

(1 100)の前の個数を求める。
(1 2)の前 = 1
(1 3)の前 = 3
(1 4)の前 = 5
(1 5)の前 = 7

(1 n) = (n-2)*2 + 1
(1 100) = (98)*2 + 1 = 197


----ここまではok
(99 100)の前の個数を求める。
(1 2) = 1
(2 3) = 4
(3 4) = 10
(4 5) = 22
(5 6) = 46
(6 7) = 94

3 6 12 24 48

(n n+1)=((n-1)*(n)) + (n-1 n)

(100 100) =

|#

(define (n-n-plus-1 n1 n2)
	(cond
		((= n1 1) 1)
		((= n1 2) 4)
		(else (+ (* (- n1 1) n1) (n-n-plus-1 (- n1 1) n1)))))

; (n-n-plus-1 1 2)
; (n-n-plus-1 2 3)
; (n-n-plus-1 3 4)
; (n-n-plus-1 4 5)
; (n-n-plus-1 5 6)
; (n-n-plus-1 99 100)

(define (place i j) ;; 対 i, j (i <= j)が何番目にくるか. (1 1)を0番とする
 (let ((s (expt 2 (- i 1))))
   (if (= i j)
       (+ s s -2)
       (+ s s -2 (* s (+ j j (- i) (- i) -1))))))

(place 1 100)
(place 99 100)
(place 100 100)