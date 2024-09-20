; 面白問

#lang racket

(define (segments->painter segment-list)
	(lambda (frame)
		(for-each
			(lambda (segment)
				(draw-line
					((frame-coord-map frame)
						(start-segment segment)) 
					((frame-coord-map frame)
						(end-segment segment))
				)
			)
		segment-list)
	)
)

; a ##
; (define (frame->painter frame)
; 	((segments->painter `((edge1-frame frame) (edge2-frame frame)))
; 		frame
; 	)
; )
; answer 輪郭とはなに？定義が詳細ではない
(define outline-painter 
	(segments->painter 
		(list (make-segment (make-vect 0 0) (make-vect 0 1))
			(make-segment (make-vect 0 1) (make-vect 1 1))
			(make-segment (make-vect 1 1) (make-vect 1 0))
			(make-segment (make-vect 1 0) (make-vect 0 0)))))
; ##

; ## b answer segmentは枠(frame)のなかに描ける線分
(define X-painter
	(segments->painter 
		(list (make-segment (make-vect 0 0) (make-vect 1 1))
			(make-segment (make-vect 1 0) (make-vect 0 1))))
)
; ##

; ## c
(define rhombus-painter
	(segments->painter 
		(list (segment (vect 0 0.5) (vect 0.5 1))
			(segment (vect 0.5 1) (vect 1 0.5))
			(segment (vect 1 0.5) (vect 0.5 0))
			(segment (vect 0.5 0) (vect 0 0.5))
		)
	)
)
; ##

; ## d answer waveって何？
; wave(手ふり人間)をひたすらsegmentでモデリングすること
(define wave-painter
	(segments->painter 
		(list (make-segment (make-vect 0.000 0.645) (make-vect 0.154 0.411))
			(make-segment (make-vect 0.154 0.411) (make-vect 0.302 0.588))
			(make-segment (make-vect 0.302 0.588) (make-vect 0.354 0.497))
			(make-segment (make-vect 0.354 0.497) (make-vect 0.245 0.000))
			(make-segment (make-vect 0.419 0.000) (make-vect 0.497 0.171))
			(make-segment (make-vect 0.497 0.171) (make-vect 0.575 0.000))
			(make-segment (make-vect 0.748 0.000) (make-vect 0.605 0.462))
			(make-segment (make-vect 0.605 0.462) (make-vect 1.000 0.142))
			(make-segment (make-vect 1.000 0.354) (make-vect 0.748 0.657))
			(make-segment (make-vect 0.748 0.657) (make-vect 0.582 0.657))
			(make-segment (make-vect 0.582 0.657) (make-vect 0.640 0.857))
			(make-segment (make-vect 0.640 0.857) (make-vect 0.575 1.000))
			(make-segment (make-vect 0.419 1.000) (make-vect 0.354 0.857))
			(make-segment (make-vect 0.354 0.857) (make-vect 0.411 0.657))
			(make-segment (make-vect 0.411 0.657) (make-vect 0.285 0.657))
			(make-segment (make-vect 0.285 0.657) (make-vect 0.154 0.605))
			(make-segment (make-vect 0.154 0.605) (make-vect 0.000 0.857)))
	)
)
; ##