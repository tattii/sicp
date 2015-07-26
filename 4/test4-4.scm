(load "ex4-4.scm")

(define env the-global-environment)

(display (eval-and '(and) env))
(display (eval-and '(and true true) env))
(display (eval-and '(and false true) env))

(display (eval-or '(or) env))
(display (eval-or '(or true false) env))
(display (eval-or '(or false false) env))

