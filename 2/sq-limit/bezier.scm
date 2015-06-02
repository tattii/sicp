(load "n-picture.scm")

;;dividing point
(define (div-point point1 point2 ratio)
  (make-vect (+ (* (- 1 ratio) (xcor-vect point1))
		(* ratio (xcor-vect point2)) )
	     (+ (* (- 1 ratio) (ycor-vect point1))
		(* ratio (ycor-vect point2)) )
))

;;bezier
;return bezier points(vectors) list
(define (bezier p0 p1 p2 p3)
  (let loop( (t 0.000) (vectors nil) )
    (if (>= t 1.000)
	vectors
	(let* ( (a (div-point p0 p1 t))
		(b (div-point p1 p2 t))
		(c (div-point p2 p3 t))

		(d (div-point a b t))
		(e (div-point b c t))

		(f (div-point d e t)) )
	      (loop (+ t 0.005)
		    (append vectors (list f)))
	 )
)))


;;line painter
(define (bezier-painter p0 p1 p2 p3)
  (vectors->painter (bezier p0 p1 p2 p3)))
