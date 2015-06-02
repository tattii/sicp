(define (inc n) (+ n 1))

(define (double term)
  (lambda (x) (term (term x))) )