(load "deriv.scm")

(deriv-print '3 'x)

(deriv-print 'x 'x)

(deriv-print '(+ x y) 'x)

(deriv-print '(- 1 x) 'x)

(deriv-print '(* x x) 'x)

(deriv-print '(/ 1 u) 'u)

(deriv-print '(** (+ (* 3 x) 1) 5) 'x)

(deriv-print '(sin (* 3 t)) 't)

(deriv-print '(cos (* -1 x)) 'x)

(deriv-print '(tan z) 'z)

(deriv-print '(exp (** x 2)) 'x)

(deriv-print '(log y) 'y)

(deriv-print '(+ x x x) 'x)

(deriv-print '(* 2 (sin x) (cos x)) 'x)
