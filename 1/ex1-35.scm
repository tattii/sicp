(define tolerance 0.00001)

(define (fixed-point f first)
  (define (close? a b)
    (< (abs (- a b)) tolerance ) )
  (define (try guess)
    (let ( (next (f guess)) )
      (if (close? guess next)
	  next
	  (try next)) ) )
  (try first) )

(define (phi)
  (define (f x) (+ 1 (/ 1 x)) )
  (fixed-point f 1) )