(load "stream-pair.scm")


(define (interleave-3 s1 s2 s3)
  (if (stream-null? s1)
	(interleave s2 s3)
	(cons-stream (stream-car s1)
				 (interleave-3 s2 s3 (stream-cdr s1)))))


(define (pairs-all s t)
  (cons-stream
	(list (stream-car s) (stream-car t))
	(interleave-3
	  (stream-map (lambda (x) (list (stream-car s) x))
				  (stream-cdr t))
	  (stream-map (lambda (x) (list x (stream-car t)))
				  (stream-cdr s))
	  (pairs-all (stream-cdr s) (stream-cdr t)))))


