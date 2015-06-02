;;必要な手続き
(define (square x) (* x x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))
))


;;タグつきデータ-----------------------------------------------------------
;;integerの効率化
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'integer)
      contents
      (cons type-tag contents)
))

;;通常の演算子と同じように使える
(define (type-tag data)
  (cond ((pair? data) (car data))
	((number? data)
	    (if (integer? data)
		'integer
		'real
	    ))
	(error "Bad Taged Data -- TYPE-TAG" data)
))

(define (contents data)
    (if (pair? data)
	(cdr data)
	data
))


;;整数------------------------------------------------------------------
(define (install-integer-package)
  ;;割り算のみ
  (define (div-integer x y)
    (let ((ans (/ x y)))
	 (if (integer? ans)
	     ans
	     (make-rational x y)
  )))

  ;;階層
  (define (raise-integer n) (make-rational n 1))
  (define (able-drop-integer n) #f)

  ;;interface
  (define (tag x) (attach-tag 'integer x))

  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))) )
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))) )
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))) )
  (put 'div '(integer integer) (tag div-integer))
  (put 'make 'integer tag)
  (put 'print '(integer) (lambda (x) (display x)))

  (put 'level 'integer 0)
  (put 'raise 'integer raise-integer)
  (put 'able-drop? 'integer able-drop-integer)

'done)

;;構成子
(define (make-integer n)
  ((get 'make 'integer) n))


;;実数------------------------------------------------------------------
(define (install-real-package)
  ;;階層
  (define (raise-real r) (make-complex-from-real-imag r 0))
  (define (able-drop-real? r) (integer? r))
  (define (drop-real r) (make-integer r))

  ;;interface
  (define (tag x) (attach-tag 'real x))

  (put 'add '(real real) (lambda (x y) (tag (+ x y))) )
  (put 'sub '(real real) (lambda (x y) (tag (- x y))) )
  (put 'mul '(real real) (lambda (x y) (tag (* x y))) )
  (put 'div '(real real) (lambda (x y) (tag (/ x y))) )
  (put 'make 'real tag)
  (put 'print '(real) (lambda (x) (display x)) )

  (put 'level 'real 2)
  (put 'raise 'real raise-real)
  (put 'able-drop? 'real able-drop-real?)
  (put 'drop 'real drop-real)

'done)

;;構成子
(define (make-real r)
  ((get 'make 'real) r))


;;有理数------------------------------------------------------------------
(define (install-rational-package)

  ;;内部手続き
  (define (make-rat n d)
    (if (< d 0)
	(and (* n -1) (* d -1))
    )
    (let ((g (gcd n d) ))
      (cons (/ n g) (/ d g))
  ))

  (define (numer q) (car q))
  (define (demon q) (cdr q))

  (define (add-rat x y)
    (make-rat
     (+ (* (numer x) (demon y)) (* (numer y) (demon x)))
     (* (demon x) (demon y))
  ))

  (define (sub-rat x y)
    (make-rat
     (- (* (numer x) (demon y)) (* (numer y) (demon x)))
     (* (demon x) (demon y))
  ))

  (define (mul-rat x y)
    (make-rat
     (* (numer x) (numer y))
     (* (demon x) (demon y))
  ))

  (define (div-rat x y)
    (make-rat
     (* (numer x) (demon y))
     (* (demon x) (numer y))
  ))

  ;;出力
  (define (print-rat x)
      (display (numer x))
      (display "/")
      (display (demon x))
  )

  ;;階層
  (define (raise-rat q) 
     (make-real (/ (numer q) (demon q)) ) )
  (define (able-drop-rat? q)
     (= (demon q) 1) )
  (define (drop-rat q)
     (make-integer (numer q)) )

  ;;interface
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))) )
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))) )
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))) )
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))) )
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))) )
  (put 'print '(rational) print-rat)

  (put 'level 'rational 1)
  (put 'raise 'rational raise-rat)
  (put 'able-drop? 'rational able-drop-rat?)
  (put 'drop 'rational drop-rat)

'done)

;;構成子
(define (make-rational n d)
  ((get 'make 'rational) n d))


;;複素数-------------------------------------------------------------------
(define (install-complex-package)
  ;;内部手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y) )

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a) )

  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))

  (define (add-complex z1 z2)
    (make-from-real-imag
        (+ (real-part z1) (real-part z2))
	(+ (imag-part z1) (imag-part z2))
  ))

  (define (sub-complex z1 z2)
    (make-from-real-imag
        (- (real-part z1) (real-part z2))
	(- (imag-part z1) (imag-part z2))
  ))

  (define (mul-complex z1 z2)
    (make-from-mag-ang
        (* (magnitude z1) (magnitude z2))
	(+ (angle z1) (angle z2))
  ))

  (define (div-complex z1 z2)
    (make-from-mag-ang
        (/ (magnitude z1) (magnitude z2))
	(- (angle z1) (angle z2))
  ))

  ;;出力
  (define (print-imag z)
      (if (= (real-part z) 0)
	  (display "")
	  (display (real-part z)) )
      (cond ((= (imag-part z) -1) (display "-i"))
	    (else
	       (if (and (> (imag-part z) 0)
			(not (= (real-part z) 0)) )
		   (display "+")
	       )
	       (if (= (imag-part z) 1)
		   (display "")
		   (display (imag-part z)) )
	       (display "i") ))
  )

  ;;階層
  (define (able-drop-complex? z)
    (= (imag-part z) 0) )
  (define (drop-complex z)
    (make-real (real-part z)) )

  ;;interface
  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))) )
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))) )
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))) )
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))) )

  (put 'make-from-real-imag 
       'complex
       (lambda (x y) (tag (make-from-real-imag x y))) )
  (put 'make-from-mag-ang 
       'complex
       (lambda (r a) (tag (make-from-mag-ang r a))) )

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'print '(complex) print-imag)

  (put 'level 'complex 3)
  (put 'able-drop? 'complex able-drop-complex?)
  (put 'drop 'complex drop-complex)

'done)

;;complex-直交座標系
(define (install-rectangular-package)

    ;;内部手続き
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))

    (define (make-from-real-imag x y) (cons x y) )

    (define (magnitude z)
        (sqrt
	   (+ (square (real-part z))
	      (square (imag-part z)) )
    ))

    (define (angle z)
        (atan (imag-part z) (real-part z)) )

    (define (make-from-mag-ang r a)
        (cons (* r (cos a))
	      (* r (sin a))
    ))

    ;;interface
    (define (tag x) (attach-tag 'rectangular x))

    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)

    (put 'make-from-real-imag
	 'rectangular
	 (lambda (x y) (tag (make-from-real-imag x y))) )

    (put 'make-from-mag-ang
	 'rectangular
	 (lambda (r a) (tag (make-from-mag-ang r a))) )

'done)

;;complex-極座標系
(define (install-polar-package)

    ;;内部手続き
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))

    (define (make-from-mag-ang r a) (cons r a) )

    (define (real-part z)
        (* (magnitude z)
	   (cos (angle z))
    ))

    (define (imag-part z)
        (* (magnitude z)
	   (sin (angle z))
    ))

    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
	      (atan y x)
    ))

    ;;interface
    (define (tag x) (attach-tag 'polar x))

    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)

    (put 'make-from-real-imag
	 'polar
	 (lambda (x y) (tag (make-from-real-imag x y))) )

    (put 'make-from-mag-ang
	 'polar
	 (lambda (r a) (tag (make-from-mag-ang r a))) )

'done)

;;構成子
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y) )

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a) )


;;読み込み----------------------------------------------------------------
(install-integer-package)
(install-real-package)

(install-rational-package)

(install-rectangular-package)
(install-polar-package)
(install-complex-package)


;;汎用手続き---------------------------------------------------------------
(define (raise arg)
  ((get 'raise (type-tag arg)) (contents arg)) )

;;簡約化
(define (project arg)
  (cond ((pair? arg)
      	     (let* ( (tag (type-tag arg))
		     (content (contents arg)) )
	           (if ((get 'able-drop? tag) content)
		       (project ((get 'drop tag) content))
		       (cons tag content)
	)))
	((number? arg) arg)
))

(define (apply-generic op . args)
    (let* ( (tags (map type-tag args))
            (proc (get op tags)) )
	  (if proc
	      (project (apply proc (map contents args)) ) ;;演算&簡約化
	      (if (= (length args) 2)
		  (let* ( (t1-level (get 'level (car tags)) )
			  (t2-level (get 'level (cadr tags)) )
			  (a1 (car args))
			  (a2 (cadr args)) )
		        (if (> t1-level t2-level) ;;階層を比較
			    (apply-generic op a1 (raise a2)) ;;型変換
			    (apply-generic op (raise a1) a2)
		  ))
		  (error "No method for these types -- APPLY-GENERIC"
			 (list op tags))
	 ))
))



;;汎用算術演算--------------------------------------------------------------
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;出力
(define (print x)
  (and (apply-generic 'print x)
       (display " : ")
       (display x)
       (newline) 
))