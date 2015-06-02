(load "n-picture.scm")

;;transform painter

;;mapping--------------------------------------
(define (rot painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)
))

(define (rot180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)
))

(define (rot270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)
))

(define (rot45 painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
))

(define (flip painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)
))


;;assemble------------------------------------------
(define (above painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5) ))
         (let ( (paint-upper
		     (transform-painter painter1
					split-point
					(make-vect 1.0 0.5)
					(make-vect 0.0 1.0)
		 ))
		 (paint-lower
		     (transform-painter painter2
					(make-vect 0.0 0.0)
					(make-vect 1.0 0.0)
					(make-vect 0.0 0.5)
		 )) )
	       (lambda (frame)
		       (paint-upper frame)
		       (paint-lower frame) )
)))

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

(define (above-trisect painter1 painter2 painter3)
  (let ( (paint-upper
	    (transform-painter painter1
			       (make-vect 0.0 0.667)
			       (make-vect 1.0 0.667)
			       (make-vect 0.0 1.0) ))
	 (paint-middle
	    (transform-painter painter2
			       (make-vect 0.0 0.333)
			       (make-vect 1.0 0.333)
			       (make-vect 0.0 0.667) ))
	 (paint-lower
	    (transform-painter painter3
			       (make-vect 0.0 0.0)
			       (make-vect 1.0 0.0)
			       (make-vect 0.0 0.333) ))
	 )
	 (lambda (frame)
	         (paint-upper frame)
		 (paint-middle frame)
		 (paint-lower frame) )
))

(define (beside-trisect painter1 painter2 painter3)
  (let ( (paint-left
	    (transform-painter painter1
			       (make-vect 0.0 0.0)
			       (make-vect 0.333 0.0)
			       (make-vect 0.0 1.0) ))
	 (paint-middle
	    (transform-painter painter2
			       (make-vect 0.333 0.0)
			       (make-vect 0.667 0.0)
			       (make-vect 0.333 1.0) ))
	 (paint-right
	    (transform-painter painter3
			       (make-vect 0.667 0.0)
			       (make-vect 1.0 0.0)
			       (make-vect 0.667 1.0) ))
	 )
	 (lambda (frame)
	         (paint-left frame)
		 (paint-middle frame)
		 (paint-right frame) )
))