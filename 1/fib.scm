(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))
))

(define (fib-iter n)
  (define (iter a b count)
    (if (= count 0 )
	b
	(iter (+ a b) a (- count 1)) ))
  (iter 1 0 n) )