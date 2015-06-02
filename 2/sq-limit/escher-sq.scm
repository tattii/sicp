(load "trans-picture.scm")


;;procedures-------------------------------
;;blank painter
(define (blank frame) #t) 

;;flip * rot45
(define (flip45 painter)
  (flip (rot45 painter)) )


;;overlay painters
(define (over painter . painters)
  (lambda (frame)
       (painter frame)
       (for-each (lambda (paint) (paint frame)) painters)
))
	

;;4 painters
(define (quartet up-l up-r low-l low-r)
  (above (beside up-l up-r)
	 (beside low-l low-r) ))

;;9 painters
(define (notet p q r s t u v w x )
  (above-trisect (beside-trisect p q r)
		 (beside-trisect s t u)
		 (beside-trisect v w x) ))


;;painters definition----------------------
(define (painter-a frame)
    (set-color 'red)
    ((vertexes->painter
        (list (make-vect 0.0 0.0)
	      (make-vect 0.0 1.0)
	      (make-vect 1.0 0.0) )
	#t)
    frame)
)

(define (painter-b frame)
    (set-color 'blue)
    ((vertexes->painter
        (list (make-vect 0.0 0.0)
	      (make-vect 0.0 1.0)
	      (make-vect 1.0 0.0) )
	#t)
    frame)
)

(define (painter-c frame)
    (set-color 'white)
    ((vertexes->painter
        (list (make-vect 0.0 0.0)
	      (make-vect 0.0 1.0)
	      (make-vect 1.0 0.0) )
	#t)
    frame)
)


;;base painters-----------------------------
(define s1
  (over (rot painter-a)
	(flip45 painter-b)
	(rot (flip45 painter-c))
))

(define s2
  (over	(rot270 (flip45 painter-a))
	(flip45 painter-b)
	painter-c
))

(define s3
  (over (rot painter-c)
	(rot (flip45 painter-a))
	(flip45 painter-b)
))

(define s4
  (over (rot180 painter-b)
	(rot180 (flip45 painter-c))
	(rot (flip45 painter-a))
))

(define cl
  (over (flip45 painter-b)
	(rot (flip45 painter-a))
	(rot180 (flip45 painter-c))
	(rot270 (flip45 painter-a))
))

(define cr
  (over (flip45 painter-b)
	(rot (flip45 painter-c))
	(rot180 (flip45 painter-b))
	(rot270 (flip45 painter-a))
))

(define center
  (over (flip45 painter-b)
	(rot (flip45 painter-a))
	(rot180 (flip45 painter-b))
	(rot270 (flip45 painter-a))
))

;;side------------------------------------
(define (side-up n)
  (if (= n 0)
      blank
      (quartet (side-up (- n 1))
	       (side-up (- n 1))
	       s1
	       s2 )
))

(define (side-left n)
  (if (= n 0)
      blank
      (quartet (side-left (- n 1))
	       s3
	       (side-left (- n 1))
	       s4 )
))

;;corner--------------------------------
(define (corner-left n)
  (if (= n 0)
      blank
      (quartet (corner-left (- n 1))
	       (side-up (- n 1))
	       (side-left (- n 1))
	       cl )
))

(define (corner-right n)
  (if (= n 0)
      blank
      (quartet (side-up (- n 1))
	       (corner-right (- n 1))
	       cr
	       (rot180 (side-left (- n 1))) )
))


;;square-limit-------------------------
(define (escher-sq-limit n)
  (if (= n 0)
      center
      (notet (corner-left n)
	     (side-up n)
	     (corner-right n)
	     (side-left n)
	     center
	     (rot180 (side-left n))
	     (rot180 (corner-right n))
	     (rot180 (side-up n))
	     (rot180 (corner-left n)) )
))



;;実行
(start-picture)

((escher-sq-limit 5) frm1)