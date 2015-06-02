(define (compose f g)
  (lambda (x) (f (g x))) )

(define (square n) (* n n))

(define (inc n) (+ n 1))