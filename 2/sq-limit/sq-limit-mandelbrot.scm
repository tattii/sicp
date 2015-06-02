(load "picture.scm")

;;split---------------------------------------------

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1)) ))
	   (beside painter (below smaller smaller)) )
))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))  ))
	   (below painter (beside smaller smaller)) )
))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ( (up (up-split painter (- n 1)) )
	     (right (right-split painter (- n 1)) ) )
	   (let ( (top-left (beside up up))
		  (bottom-right (below right right))
		  (corner (coner-split painter (- n 1))) )
	        (beside (below painter top-left)
			(below bottom-right corner))
))))


;;square-limit--------------------------------------

(define (square-limit painter n)
  (let ((quarter (corner-split painter n) ))
       (let ((half (beside (flip-horiz quarter)
			   quarter) ))
	    (below (flip-vert half)
		   half)
)))



;;実行------------------------------------------------
(start-picture)

(load "mandelbrot.scm")

(define mandel-painter (procedure->painter mandelbrot))

((square-limit mandel-painter 5) frm1)