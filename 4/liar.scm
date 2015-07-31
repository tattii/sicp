(define (require p) (if (not p) (amb)))

(define (right? honest says n)
  (or
	(and (= honest 0) (not (= says n)))
	(and (= honest 1) (= says n))))


(define (liar)
  (let ((a (amb 0 1))
		(b (amb 0 1))
		(c (amb 0 1))
	 	(d (amb 0 1))
		(e (amb 0 1)))
	(let ((n (+ a b c d e)))
	  (require (right? a 4 n))
	  (require (right? b 3 n))
	  (require (right? c 2 n))
	  (require (right? d 1 n))
	  (require (right? e 0 n))
	  (list a b c d e))))

