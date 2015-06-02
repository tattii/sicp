(define (cube n)
  (* n n n))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))
))

(define (simpson f a b n)
  (define h (/ (- b a) n) )
  (define (y k) (f (+ a (* k h))) )
  (define (f-simp k) (+ (y (- k 1)) (* 4 (y k)) (y (+ k 1)) ))
  (define (next k) (+ k 2) )
  (/ (* (sum f-simp 1 next (- n 1) ) h) 3)
)


(define (integral f a b dx)
  (define (add-dx x) (+ x dx) )
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
)

