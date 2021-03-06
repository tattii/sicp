(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	(error "Insufficient funds") ))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
	   balance) )
  (define (dispatch p m)
    (if (eq? m 'joint)
	(eq? p password)
	(if (eq? p password)
	    (cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE_ACCOUNT")) )
	    (error "Incorrect password") )))
  dispatch)


(define (make-joint account pass new-pass)
  (define (dispatch p m)
    (if (eq? p new-pass)
	(account pass m)
	(error "Incorrect password") ))
  (if (account 'joint pass)
      dispatch
      (error "Incorrect password")) )