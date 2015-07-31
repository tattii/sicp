;; ex4.11

(define (make-frame pairs) pairs)

(define (add-binding-to-frame! var val frame)
  (define (loop pairs)
	(if (null? (cdr pairs))
	  (set-cdr! pairs (list (cons var val)))
	  (loop (cdr pairs))))
  (loop frame))
  
(define (extend-environment pairs base-env)
  (cons (make-frame pairs) base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan pairs)
      (cond ((null? pairs)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar pairs))
             (cdar pairs))
            (else (scan (cdr pairs)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan pairs)
      (cond ((null? pairs)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar pairs))
             (set-cdr! (car pairs) val))
            (else (scan (cdr pairs)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan pairs)
      (cond ((null? pairs)
             (add-binding-to-frame! var val frame))
            ((eq? var (caar pairs))
             (set-cdr! (car pairs) val))
            (else (scan (cdr pairs)))))
    (scan frame)))


(define (setup-environment)
  (let ((initial-env
         (extend-environment 
		   primitive-procedures
           the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (cons 'car car)
        (cons 'cdr cdr)
        (cons 'cons cons)
        (cons 'null? null?)
;;        <more primitives>
        ))
