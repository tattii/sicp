(load "picture.scm")

;;color gradation 
(load "grad-list.scm")

(define (gradation n)
  (cond ((= n 0) #x000000)
	((> n 64) #xFFFFFF)
	(else (list-ref grad-list (- 65 n)))
))

;;mandelbrot set
(define (mandelbrot z)
    (let ( (x0 (+ (* 3.0 (car z)) -2.0))
	   (y0 (+ (* 3.0 (cdr z)) -1.5)) )
      (let loop( (xn x0) (yn y0) (i 1) )
	(let ((abs (- (* xn xn) (* yn yn )) ))
	  (cond ((> i 100) (gradation 0))
		((> abs 4.0) (gradation i))
		(else (loop (+ abs x0)
			    (+ (* 2.0 xn yn) y0)
			    (+ i 1)) )
      )))
))
