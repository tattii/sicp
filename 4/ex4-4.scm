(load "m-eval.scm")
;
; logical operation
;

(define (logical-exps exp) (cdr exp))

(define (logical-first-exp exps) (car exps))
(define (logical-rest-exps exps) (cdr exps))
(define (logical-last-exp? exps) (null? (cdr exps)))


;; and
(define (and? exp) (tagged-list? exp 'and))

(define (eval-and exp env)
  (define (and-iter exps)
	(cond ((null? exps) true)
	      ((logical-last-exp? exps)
		   (eval (logical-first-exp exps) env))
		  ((false? (eval (logical-first-exp exps) env))
		   false)
		  (else
			(and-iter (logical-rest-exps exps)))))
  (and-iter (logical-exps exp)))


;; or
(define (or? exp) (tagged-list? exp 'or))

(define (eval-or exp env)
  (define (or-iter exps)
	(cond ((null? exps) false)
		  ((logical-last-exp? exps)
		   (eval (logical-first-exp exps) env))
		  ((true? (eval (logical-first-exp exps) env))
		   (eval (logical-first-exp exps) env))
		  (else
			(or-iter (logical-rest-exps exps)))))
  (or-iter (logical-exps exp)))

