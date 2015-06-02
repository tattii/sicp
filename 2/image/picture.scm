 ;;vector----------------------------------------
;constructor
(define (make-vect x y) (cons x y))

;selector
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

;operation
(define (add-vect v w)
    (make-vect (+ (xcor-vect v) (xcor-vect w))
	       (+ (ycor-vect v) (ycor-vect w))
))

(define (sub-vect v w)
    (make-vect (- (xcor-vect v) (xcor-vect w))
	       (- (ycor-vect v) (ycor-vect w))
))

(define (scale-vect s v)
    (make-vect (* s (xcor-vect v))
	       (* s (ycor-vect v))
))

;convert to segment
(define (vectors->segments vector-list)
  (define (iter vectors joint segments)
    (if (null? vectors)
	segments
	(iter (cdr vectors)
	      (car vectors)
	      (append (list (make-segment joint (car vectors)))
		      segments) )
   ))
  (iter (cdr vector-list) (car vector-list) nil)
)

;;segment----------------------------------------
;constructor
(define (make-segment from to)
  (cons from to))

;selector
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))


;;frame-----------------------------------------
;constructor
(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

;selector
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

;mapping
(define (frame-coord-map frame)
    (lambda (v)
         (add-vect (origin-frame frame)
		   (add-vect (scale-vect (xcor-vect v)
					 (edge1-frame frame))
			     (scale-vect (ycor-vect v)
					 (edge2-frame frame))
))))

;;painter----------------------------------------

;transform
(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-coord-map frame) ))
	     (let ((new-origin (m origin) ))
	          (painter (make-frame new-origin
				       (sub-vect (m corner1) new-origin)
				       (sub-vect (m corner2) new-origin)
))))))

(define (flip-vert painter)
    (transform-painter painter
		       (make-vect 0.0 1.0)
		       (make-vect 1.0 1.0)
		       (make-vect 0.0 0.0)
))

(define (flip-horiz painter)
    (transform-painter painter
		       (make-vect 1.0 0.0)
		       (make-vect 0.0 0.0)
		       (make-vect 1.0 1.0)
))

;vertexes
(define (vertexes->painter vertex-list fill?)
    (lambda (frame)
            (draw-polygon
	         (map (frame-coord-map frame) vertex-list)
		 fill?
)))

;segments
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

;vectors
(define (vectors->painter vector-list)
   (segments->painter
      (vectors->segments vector-list) ))


;;assemble------------------------------------------
(define (beside painter1 painter2)
    (let ((split-point (make-vect 0.5 0.0) ))
         (let ( (paint-left
		     (transform-painter painter1
					(make-vect 0.0 0.0)
					split-point
					(make-vect 0.0 1.0)
		 ))
		 (paint-right
		     (transform-painter painter2
					split-point
					(make-vect 1.0 0.0)
					(make-vect 0.5 1.0)
		 )) )
	       (lambda (frame)
		       (paint-left frame)
		       (paint-right frame) )
)))

(define (below painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5) ))
         (let ( (paint-lower
		     (transform-painter painter1
					(make-vect 0.0 0.0)
					(make-vect 1.0 0.0)
					split-point
		 ))
		 (paint-upper
		     (transform-painter painter2
					split-point
					(make-vect 1.0 0.5)
					(make-vect 0.0 1.0)
		 )) )
	       (lambda (frame)
		       (paint-lower frame)
		       (paint-upper frame) )
)))






;;normal frame
(define frm1
  (make-frame (make-vect 0.0 0.0)
	      (make-vect 1.0 0.0)
	      (make-vect 0.0 1.0)
))