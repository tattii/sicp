;;; amb-eval への道:
;;; (1) interpret (amb e1 ... en) as
;;;     (catch e1 (lambda (x1) 
;;;                 (catch e2 ... 
;;;                      (catch en (lambda (xn) (throw 'fail)))))
;;;   → これだと最初に計算が成功したところで終わってしまう!
;;;
;;; e1 の計算が終わっても e2 の計算に戻る必要あり

;;; 教科書のコードへの道
;;; 1. continuation を functionalize する
;;; 2. 4.1.7 のテクニックを適用

;;; evaluator in CPS with continuation as concrete data
(define (eval exp env cont fcont)
  ;; FCONT stands for a continuation to be invoked on failrue
  (cond
   ((self-evaluating? exp) (apply-cont cont exp fcont))
   ((variable? exp) (apply-cont cont (lookup-variable-value exp env) fcont))
   ((quoted? exp) (apply-cont cont (text-of-quotation exp) fcont))
   ((assignment? exp) (eval-assignment exp env cont fcont))
   ;; Only top-level define works.
   ;; Exercise 4.16 makes internal definitions work properly.
   ((definition? exp) (eval-definition exp env cont fcont))
   ((if? exp) (eval-if exp env cont fcont))
   ((or? exp) (eval (or->if exp) env cont fcont))
   ((and? exp) (eval (and->if exp) env cont fcont))
   ((let? exp) (eval (expand-let exp) env cont fcont))
   ((lambda? exp)
    (apply-cont 
     cont
     (make-procedure (lambda-parameters exp)
		     (lambda-body exp)
		     env)
     fcont))
   ((begin? exp) 
    (eval-sequence (begin-actions exp) env cont fcont))
   ((cond? exp) (eval (cond->if exp) env cont fcont))
   ((amb? exp) (eval-amb (amb-choices exp) env cont fcont))
   ((application? exp)
    (eval (operator exp) env
	  (make-operandsc (operands exp) env cont)
	  fcont))
   (else
    (error "Unknown expression type -- EVAL" exp))))

(define (my-apply procedure arguments cont fcont)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments cont fcont))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))
	   cont
	   fcont))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (apply-cont cont val fcont)
  (cond ((haltc? cont) (list 'normal val fcont))
	((testc? cont)
	 (if (true? val)
	     (eval (testc-true cont) (testc-env cont) (testc-cont cont) fcont)
	     (eval (testc-false cont) (testc-env cont) (testc-cont cont) fcont)))
	((assignc? cont)
	 (let* ((var (assignc-var cont))
		(env (assignc-env cont))
		(oldval (lookup-variable-value var env)))
	   (set-variable-value! var val env)
	   (apply-cont (assignc-cont cont) 'ok 
		       ;; recording information required to revert
		       ;; the assignment in fcont
		       (make-revassignc var oldval env fcont))))
	((definec? cont)
	 (define-variable! (definec-var cont) val (definec-env cont))
	 (apply-cont (definec-cont cont) 'ok fcont))
	((beginc? cont)
	 (eval-sequence
	  (beginc-rest-exps cont)
	  (beginc-env cont)
	  (beginc-cont cont)
	  fcont))
	((operandsc? cont)
	 (list-of-values (operandsc-exps cont) 
			 (operandsc-env cont)
			 (make-applyc val (operandsc-cont cont))
			 fcont))
	((applyc? cont)
	 (my-apply (applyc-proc cont) val (applyc-cont cont) fcont))
	((restopsc? cont)
	 (list-of-values (restopsc-rest cont) (restopsc-env cont)
			 (make-consc val (restopsc-cont cont))
			 fcont))
	((consc? cont)
	 (apply-cont (consc-cont cont)
		     (cons (consc-val cont) val) fcont))
	(else (error "Unknown continuation type -- APPLY-CONT" cont))))

(define (apply-fcont fcont)
  (cond
   ((initf2c? fcont) (list 'no-problem))
   ((initf1c? fcont) (list 'no-more-val (initf1c-exp fcont)))
   ((ambc? fcont)
    (eval-amb (ambc-exps fcont) (ambc-env fcont) 
	      (ambc-cont fcont) (ambc-fcont fcont)))
   ((revassignc? fcont)
    (set-variable-value! (revassignc-var fcont)
			 (revassignc-old fcont)
			 (revassignc-env fcont))
    (apply-fcont (revassignc-fcont fcont)))
   (else (error "Unknown failure continuation type -- APPLY-FCONT" fcont))
))

(define (list-of-values exps env cont fcont)
  (if (no-operands? exps)
      (apply-cont cont '() fcont)
      (eval (first-operand exps) env
	    (make-restopsc (rest-operands exps) env cont)
	    fcont)))

(define (eval-if exp env cont fcont)
  (eval (if-predicate exp) env
	(make-testc (if-consequent exp) (if-alternative exp) env cont)
	fcont))

(define (eval-sequence exps env cont fcont)
  (cond ((last-exp? exps) (eval (first-exp exps) env cont fcont))
        (else (eval (first-exp exps) env
		    (make-beginc (rest-exps exps) env cont)
		    fcont))))

(define (eval-assignment exp env cont fcont)
  (eval (assignment-value exp) env
	(make-assignc (assignment-variable exp) env cont)
	fcont))

(define (eval-definition exp env cont fcont)
  (eval (definition-value exp) env
	(make-definec (definition-variable exp) env cont)
	fcont))

(define (eval-amb choices env cont fcont)
  (if (null? choices)
      (apply-fcont fcont)
      ;; fcont should be ambc, revassignc, or haltc, which will discard
      ;; the second and third arguments in apply-cont
      (eval (car choices) env cont
	    (make-ambc (cdr choices) env cont fcont))))

;;; Sec 4.1.2
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (or? exp) (tagged-list? exp 'or))
(define (or-choices exp) (cdr exp))

(define (or->if exp)
  (if (null? (or-choices exp)) 'false
      `(let ((_tmp_ ,(car (or-choices exp))))
	 (if _tmp_ _tmp_ (or ,@(cdr (or-choices exp)))))))

(define (and? exp) (tagged-list? exp 'and))
(define (and-choices exp) (cdr exp))

(define (and->if exp)
  (if (null? (and-choices exp)) 'true
      `(let ((_tmp_ ,(car (and-choices exp))))
	 (if _tmp_ (and ,@(cdr (and-choices exp)))
	     _tmp_))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars bindings)
  (if (null? bindings) '()
      (cons (caar bindings) (let-vars (cdr bindings)))))
(define (let-vals bindings)
  (if (null? bindings) '()
      (cons (cadar bindings) (let-vals (cdr bindings)))))
(define (expand-let exp)
  `((lambda ,(let-vars (let-bindings exp))
      ,@(let-body exp))
    ,@(let-vals (let-bindings exp))))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; AMB
(define (amb? exp) (eq? (car exp) 'amb))
(define (amb-choices exp) (cdr exp))
;; AMB END

;;; data representation for continuations
(define (make-haltc) 'haltc)
(define (haltc? cont) (eq? cont 'haltc))

(define (make-testc true-exp false-exp env cont)
  (list 'testc true-exp false-exp env cont))
(define (testc? cont) (tagged-list? cont 'testc))
(define (testc-true cont) (cadr cont))
(define (testc-false cont) (caddr cont))
(define (testc-env cont) (cadddr cont))
(define (testc-cont cont) (car (cddddr cont)))

(define (make-beginc exps env cont)
  (list 'beginc exps env cont))
(define (beginc? cont) (tagged-list? cont 'beginc))
(define (beginc-rest-exps cont) (cadr cont))
(define (beginc-env cont) (caddr cont))
(define (beginc-cont cont) (cadddr cont))

(define (make-assignc var env cont) (list 'assignc var env cont))
(define (assignc? cont) (tagged-list? cont 'assignc))
(define (assignc-var cont) (cadr cont))
(define (assignc-env cont) (caddr cont))
(define (assignc-cont cont) (cadddr cont))

(define (make-definec var env cont) (list 'definec var env cont))
(define (definec? cont) (tagged-list? cont 'definec))
(define (definec-var cont) (cadr cont))
(define (definec-env cont) (caddr cont))
(define (definec-cont cont) (cadddr cont))

(define (make-operandsc exps env cont)
  (list 'operandsc exps env cont))
(define (operandsc? cont) (tagged-list? cont 'operandsc))
(define (operandsc-exps cont) (cadr cont))
(define (operandsc-env cont) (caddr cont))
(define (operandsc-cont cont) (cadddr cont))

(define (make-applyc proc cont)
  (list 'applyc proc cont))
(define (applyc? cont) (tagged-list? cont 'applyc))
(define (applyc-proc cont) (cadr cont))
(define (applyc-cont cont) (caddr cont))

(define (make-restopsc exps env cont)
  (list 'restopsc exps env cont))
(define (restopsc? cont) (tagged-list? cont 'restopsc))
(define (restopsc-rest cont) (cadr cont))
(define (restopsc-env cont) (caddr cont))
(define (restopsc-cont cont) (cadddr cont))

(define (make-consc val cont)
  (list 'consc val cont))
(define (consc? cont) (tagged-list? cont 'consc))
(define (consc-val cont) (cadr cont))
(define (consc-cont cont) (caddr cont))

;;; AMB
(define (make-ambc exps env cont fcont) (list 'ambc exps env cont fcont))
(define (ambc? cont) (eq? (car cont) 'ambc))
(define (ambc-exps cont) (cadr cont))
(define (ambc-env cont) (caddr cont))
(define (ambc-cont cont) (cadddr cont))
(define (ambc-fcont cont) (car (cddddr cont)))

(define (make-revassignc var old env fcont)
  (list 'revassignc var old env fcont))
(define (revassignc? cont) (tagged-list? cont 'revassignc))
(define (revassignc-var cont) (cadr cont))
(define (revassignc-old cont) (caddr cont))
(define (revassignc-env cont) (cadddr cont))
(define (revassignc-fcont cont) (car (cddddr cont)))

(define (make-initf1c exp) (list 'initf1c exp))
(define (initf1c? cont) (tagged-list? cont 'initf1c))
(define (initf1c-exp cont) (cadr cont))

(define (make-initf2c) '(initf2c))
(define (initf2c? cont) (tagged-list? cont 'initf2c))
;;; AMB END

;;; Sec. 4.1.3
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;; Sec. 4.1.4
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list 'zero? zero?)
	(list 'list list)
	(list '> >)
	(list '< <)
	(list '>= >=)
	(list '<= <=)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '= =)
	(list '/ /)
	(list 'abs abs)
	(list 'not not)
	(list 'memq memq)
	(list 'remainder remainder)
	(list 'eq? eq?)
;;        <more primitives>
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args cont fcont)
  (apply-cont cont
	      (apply (primitive-implementation proc) args)
	      fcont))

(define input-prompt ";;; AMB-Eval input:")
(define output-prompt ";;; AMB-Eval value:")
(define (driver-loop)
  (define (loop fcont)
    (prompt-for-input input-prompt)
    (let* ((input (read))
	   (output 
	    (if (eq? input 'try-again)
		(apply-fcont fcont)
		(begin
		  (newline)
		  (display ";;; Starting a new problem")
		  (eval input the-global-environment
			 (make-haltc)
			 (make-initf1c input))))))
      (cond ((tagged-list? output 'normal)
	     (announce-output output-prompt)
	     (user-print (cadr output))
	     (loop (caddr output)))
	    ((tagged-list? output 'no-problem)
	     (announce-output ";;; There is no current problem")
	     (loop (make-initf2c)))
	    ((tagged-list? output 'no-more-val)
	     (announce-output ";;; There are no more values of ")
	     (user-print (cadr output))
	     (loop (make-initf2c))))))
  (loop (make-initf2c)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
