
(load "constraints.scm")

(define (averager a b c)
  (let ((eq (make-connector))
	(m1 (make-connector)))
    (multiplier m1 c eq)
    (adder a b eq)
    (constant 2 m1)
    (probe "a" a)
    (probe "b" b)
    (probe "average" c)
    'ok))