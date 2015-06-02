(define (product term a next b)
  (if (> a b) 1
      (* (term a) (product term (next a) next b))
))

(define (inc n)
  (+ n 1))

(define (factorial n)
  (define (f x) x)
  (product f 1 inc n))

(define (pi n)
  (define (f n) 
    (define x (+ (* n 2) 1))
    (/ (* (- x 1) (+ x 1)) (* x x) ) )
  (* (product f 1 inc n) 4) )


(define (product-iter term a next b)
  (define (iter a product)
    (if (> a b)
	product
	(iter (next a) (* product (term a) )) ))
    (iter a 1))

(define (fact-iter n)
  (define (f x) x)
  (product-iter f 1 inc n))