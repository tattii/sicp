;;3.3.2 Queue

;;operation
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))

;;constructor
(define (make-queue) (cons '() '()) )

;;selector
(define (empty-queue? queue)
  (null? (front-ptr queue)) )

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Empty Queue")
      (car (front-ptr queue))
))

;;mutator
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond
     ((empty-queue? queue)
      (set-front-ptr! queue new-pair)
      (set-rear-ptr! queue new-pair)
      queue)
     (else
      (set-cdr! (rear-ptr queue) new-pair)
      (set-rear-ptr! queue new-pair)
      queue)
)))

(define (delete-queue! queue)
  (cond
    ((empty-queue? queue) (error "Empty Queue"))
    (else (set-front-ptr! queue (cdr (front-ptr queue)))
	  queue)
))