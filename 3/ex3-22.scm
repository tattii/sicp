;; ex3.22 Queue

(define (make-queue)
  (let ((front-ptr '())
		(rear-ptr '()))

	(define (set-front-ptr! item) (set! front-ptr item))
	(define (set-rear-ptr! item) (set! rear-ptr item))

	;; selector
	(define (empty-queue?) (null? front-ptr))

	(define (front-queue)
	  (if (empty-queue?)
		(error "Empty Queue")
		front-ptr))

	;; mutator
	(define (insert-queue! item)
	  (let ((new-pair (cons item '())))
		(cond
		  ((empty-queue?)
		   (set-front-ptr! new-pair)
		   (set-rear-ptr! new-pair)
		   front-ptr)
		  (else
			(set-cdr! rear-ptr new-pair)
			(set-rear-ptr! new-pair)
			front-ptr))))

	(define (delete-queue!)
	  (cond
		((empty-queue?) (error "Empty Queue"))
		(else (let ((del (car front-ptr)))
		  	(set-front-ptr! (cdr front-ptr))
			del))))

	(define (dispatch m)
	  (cond ((eq? m 'empty-queue?) empty-queue?)
		    ((eq? m 'front-queue) front-queue)
		    ((eq? m 'insert!) insert-queue!)
			((eq? m 'delete!) delete-queue!)
			(else (error "Unknown request" m))))
	dispatch))

(define (empty-queue? queue) ((queue 'empty-queue?)))
(define (front-queue queue) ((queue 'front-queue)))
(define (insert-queue! queue item) ((queue 'insert!) item))
(define (delete-queue! queue) ((queue 'delete!)))
