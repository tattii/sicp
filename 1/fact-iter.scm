(define (fact-iter n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* product counter)
	      (+ counter 1)) ))
  (iter 1 1) )