;;変数？
(define (variable? x) (symbol? x))

;;同じ変数？
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)
))

;;式が数に等しいか？
(define (=number? exp num)
  (and (number? exp)
       (= exp num)
))


;;和---------------------------------------------
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))
))

;;和のリスト？
(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)
))

;;加数は第2項
(define (addend s) (cadr s))

;;被加数は第3項以降
(define (augend s)
  (if (pair? (cdddr s))
      (cons '+ (cddr s))
      (caddr s)
))


;;差-----------------------------------------------
;;差のリスト？
(define (sub? x)
  (and (pair? x)
       (eq? (car x) '-)
))

;;減数は(* -1 第3項)
(define (subtrahend s)
  (make-product -1
		(caddr s)
))

;;被減数は第2項
(define (minuend s) (cadr s))


;;積-----------------------------------------------
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))
))

;;積のリスト？
(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)
))

;;乗数は第2項
(define (multiplier p) (cadr p))

;;被乗数は第3項以降
(define (multiplicand p)
  (if (pair? (cdddr p))
      (cons '* (cddr p))
      (caddr p)
))


;;商-----------------------------------------------
(define (make-division d1 d2)
  (cond ((=number? d1 0) 0)
	((=number? d2 1) d1)
	((and (number? d1) (number? d2)) (/ d1 d2))
	(else (list '/ d1 d2))
))

;;商のリスト？
(define (division? x)
  (and (pair? x)
       (eq? (car x) '/)
))

;;被除数は第2項
(define (divident d) (cadr d))

;;除数は第3項
(define (divisor d) (caddr d))


;;累乗---------------------------------------------
(define (** b e)
  (if (= e 0)
      1
      (* b (** b (- e 1)))
))

(define (make-exponent b e)
  (cond ((=number? b 1) 1)
	((=number? e 0) 1)
	((=number? e 1) b)
	((and (number? b) (number? e)) (** b e))
	(else (list '** b e))
))
  
;;累乗のリスト？
(define (exponent? x)
  (and (pair? x)
       (eq? (car x) '**)
))

;;底は第2項
(define (base e) (cadr e))

;;指数は第3項
(define (exponent e) (caddr e))


;;関数---------------------------------------------
;;sin,cos,tan,exp,logを定義する
(define (make-function func . args)
  (cons func args))

;;関数？
(define (func? func x)
  (and (pair? x)
       (eq? (car x) func)
))

(define (sin? x) (func? 'sin x))
(define (cos? x) (func? 'cos x))
(define (tan? x) (func? 'tan x))
(define (exp? x) (func? 'exp x))
(define (log? x) (func? 'log x))

;;引数
(define (argument f) (cadr f))


;;微分規則------------------------------------------
(define (deriv exp var)
  (cond
        ;;定数
        ((number? exp) 0)
        ;;変数のみ
	((variable? exp)
	     (if (same-variable? exp var) 1 0))
	;;和
	((sum? exp)
	     (make-sum (deriv (addend exp) var)
		       (deriv (augend exp) var)) )
	;;差
	((sub? exp)
	     (make-sum (deriv (minuend exp) var)
		       (deriv (subtrahend exp) var)) )
	;;積
	((product? exp)
	     (make-sum
	           (make-product (multiplier exp)
				 (deriv (multiplicand exp) var))
		   (make-product (multiplicand exp)
				 (deriv (multiplier exp) var)) ))
	;;商
	((division? exp)
	     (make-sum
	          (make-division
		      (make-product 
		          (make-product -1
				        (divident exp))
			  (deriv (divisor exp) var))
		      (make-product (divisor exp)
				    (divisor exp))
		  )
		  (make-product
		      (make-division 1
				     (divisor exp))
		      (deriv (divident exp) var)
		  )))
	;;累乗
	((exponent? exp)
	    (make-product
	        (make-product (exponent exp)
			      (make-exponent (base exp) (- (exponent exp) 1)) )
		(deriv (base exp) var) ))
	;;sin
	((sin? exp)
	    (make-product
	        (make-function 'cos (argument exp))
		(deriv (argument exp) var) ))
	;;cos
	((cos? exp)
	    (make-product
	        (make-function 'sin (argument exp))
		(make-product -1
			      (deriv (argument exp) var) ) ))
	;;tan
	((tan? exp)
	    (make-division 
	        (deriv (argument exp) var)
		(make-product
		    (make-function 'cos (argument exp))
		    (make-function 'cos (argument exp)) ) ))
	;;exp
	((exp? exp)
	    (make-product
	        (make-function 'exp (argument exp))
		(deriv (argument exp) var) ))
	;;log
	((log? exp)
	     (make-division
		(deriv (argument exp) var)
		(argument exp) ))
	;;定義なし
	(else (error "unknown expression type -- DERIV" exp))
 ))


;;出力
(define (deriv-print  exp var)
  (display "d/d")
  (display var)
  (display " ")
  (display exp)
  (display " = ")
  (display (deriv exp var))
  (newline)
)