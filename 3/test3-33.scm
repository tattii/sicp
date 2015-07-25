(load "ex3-33.scm")

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(averager A B C)

(set-value! A 2 'user)
(set-value! B 8 'user)

(forget-value! A 'user)

(set-value! A -4 'user)