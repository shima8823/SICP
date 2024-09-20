; 面白問
; 実行できてない

#lang racket

; ## a
(define wave-painter
	(segments->painter 
		(list (segment (vect 0.000 0.645) (vect 0.154 0.411))
			(segment (vect 0.154 0.411) (vect 0.302 0.588))
			(segment (vect 0.302 0.588) (vect 0.354 0.497))
			(segment (vect 0.354 0.497) (vect 0.245 0.000))
			(segment (vect 0.419 0.000) (vect 0.497 0.171))
			(segment (vect 0.497 0.171) (vect 0.575 0.000))
			(segment (vect 0.748 0.000) (vect 0.605 0.462))
			(segment (vect 0.605 0.462) (vect 1.000 0.142))
			(segment (vect 1.000 0.354) (vect 0.748 0.657))
			(segment (vect 0.748 0.657) (vect 0.582 0.657))
			(segment (vect 0.582 0.657) (vect 0.640 0.857))
			(segment (vect 0.640 0.857) (vect 0.575 1.000))
			(segment (vect 0.419 1.000) (vect 0.354 0.857))
			(segment (vect 0.354 0.857) (vect 0.411 0.657))
			(segment (vect 0.411 0.657) (vect 0.285 0.657))
			(segment (vect 0.285 0.657) (vect 0.154 0.605))
			(segment (vect 0.154 0.605) (vect 0.000 0.857))
			(segment (vect 0.47 0.75) (vect 0.53 0.75))
			(segment (vect 0.45 0.78) (vect 0.475 0.75))
			(segment (vect 0.55 0.78) (vect 0.527 0.75)))
	)
)
; ##

; ## b

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2)
	(make-vect
		(+ (xcor-vect v1) (xcor-vect v2))
		(+ (ycor-vect v1) (ycor-vect v2))
	)
)

(define (sub-vect v1 v2)
	(make-vect
		(- (xcor-vect v1) (xcor-vect v2))
		(- (ycor-vect v1) (ycor-vect v2))
	)
)

(define (scale-vect v s)
	(make-vect
		(* (xcor-vect v) s)
		(* (ycor-vect v) s)
	)
)

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))

(define (frame-coord-map frame)
	(lambda (v)
		(add-vect
			(origin-frame frame)
			(add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
					  (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (transform-painter painter origin corner1 corner2)
	(lambda (frame)
		(let ((m (frame-coord-map frame)))
			(let ((new-origin (m origin)))
				(painter 
					(make-frame
						new-origin
						(sub-vect (m corner1) new-origin)
						(sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
	(let ((split-point (make-vect 0.5 0.0)))
		(let
			(
				(paint-left 
					(transform-painter
						painter1
						(make-vect 0.0 0.0)
						split-point
						(make-vect 0.0 1.0))
				)
				(paint-right 
					(transform-painter
						painter2
						split-point
						(make-vect 1.0 0.0)
						(make-vect 0.5 1.0))
				)
			)
			(lambda (frame)
				(paint-left frame)
				(paint-right frame)
			)
		)
	)
)

(define (rotate90 painter)
	(transform-painter painter
		(make-vect 1.0 0.0)
		(make-vect 1.0 1.0)
		(make-vect 0.0 0.0)))

(define (rotate270 painter)
	(transform-painter painter
		(make-vect 0.0 1.0)
		(make-vect 0.0 0.0)
		(make-vect 1.0 1.0)))

(define (below painter1 painter2)
	(rotate90 (beside (rotate270 painter1) (rotate270 painter2)))
)

(define (split f1 f2)
	(lambda (painter n)
		(if (= n 0)
			painter
			(let
				((smaller ((split f1 f2) painter (- n 1))))
				(f2 painter (f1 smaller smaller))
			)
		)
	)
)

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
	(if (= n 0)
		painter
		(let
			((up (up-split painter (- n 1)))
			 (right (right-split painter (- n 1))))
				(let
					((top-left (beside up up))
					(bottom-right (below up up))
					(corner (corner-split painter (- n 1))))
					
					(beside (below painter top-left)
							(below bottom-right corner))
				)
		)
	)
)

; ## c

(define (corner-split painter n)
	(if (= n 0)
		(flip-horiz painter)
		(let ((up (up-split painter (- n 1)))
				(right (right-split painter (- n 1))))
			(let ((top-left (beside up up))
				(bottom-right (below right right))
				(corner (corner-split painter (- n 1))))
			(beside (below painter top-left)
					(below bottom-right corner))))))


(define (square-limit painter n)
	(let ((combine4 (square-of-four flip-horiz identity
									rotate180 flip-vert)))
	(combine4 (corner-split painter n))))