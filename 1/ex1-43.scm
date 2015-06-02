(define (repeated f n)
  (if (< n 1) (lambda (x) x)
      (compose f (repeated f (- n 1)) )
))

(define (square n) (* n n))