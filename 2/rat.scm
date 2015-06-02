(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (demon x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (demon x))
  x
)

(define (add-rat x y)
  (make-rat
   (+ (* (numer x) (demon y)) (* (numer y) (demon x)))
   (* (demon x) (demon y))
  ))

(define (sub-rat x y)
  (make-rat
   (- (* (numer x) (demon y)) (* (numer y) (demon x)))
   (* (demon x) (demon y))
  ))

(define (mul-rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (demon x) (demon y))
  ))

(define (div-rat x y)
  (make-rat
   (* (numer x) (demon y))
   (* (demon x) (numer y))
  ))

(define (equal-rat? x y)
  (if (= (* (numer x) (demon y))
	 (* (demon y) (numer x)))
      #t
      #f
))
