;;3.5 Stream

;;special form
;(define (cons-stream a b)
;  (cons a (delay b)) )

;(define (delay x) (memo-proc (lambda () x)))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))



(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define (stream-null? s) (null? s))


;;operations
(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))
))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car streams))
       (apply stream-map
              (cons proc (map stream-cdr streams))) )
))

(define (stream-filter pred? stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred? (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred?
				     (stram-cdr stream)) ))
	(else (stream-filter pred? (stream-cdr stream)))
))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ low 1) high))
))

(define (stream-for-each proc stream)
  (if (stream-null? stream)
      'ok
      (begin (proc (stream-car stream))
	     (stream-for-each proc (stream-cdr stream)))
))


;;display stream
(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (display x)
  (newline))

;Stereamのn番目までを出力
(define (display-stream-n s n)
  (define (display-iter i)
    (if (= i n)
	'done
	(begin (display (stream-ref s i))
	       (newline)
	       (display-iter (+ i 1)) )))
  (display-iter 0)
)

;;infinite stream
(define (integers-start-from n)
  (cons-stream n (integers-start-from (+ n 1))))

(define integers
  (integers-start-from 1))

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))
