(load "picture.scm")

(define (gasket p0 q0 p1 q1 i)
  (if (= i 0)
      (let* ( (xm (/ (+ p0 p1) 2.0))
	      (ym (+ q0 (* 0.866 (- p1 p0)))) )
	    (list (make-vect p0 q0)
		  (make-vect p1 q1) 
		  (make-vect xm ym) 
		  (make-vect p0 q0) ))
      (let* ( (xm (/ (+ p0 p1) 2.0))
	      (ym (+ (* (- p1 p0) 0.866) q0))
	      (x1 (/ (+ (* 3.0 p0) p1) 4.0))
	      (x2 (/ (+ (* 3.0 p1) p0) 4.0))
	      (y1 (+ (* (- p1 p0) 0.433) q0)) )
	    (append (gasket p0 q0 xm q0 (- i 1))
		    (gasket xm q0 p1 q0 (- i 1))
		    (list (make-vect p0 q0))
		    (gasket x1 y1 x2 y1 (- i 1))
		    (list (make-vect p0 q0)) ))
))


(define (sierpinski n)
     (vectors->painter (gasket 0.0 0.0 1.0 0.0 n) ))



;;実行
(start-picture)

((sierpinski 6) frm1)