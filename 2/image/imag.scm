;;タグつきデータ
(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (type-tag data)
    (if (pair? data)
	(car data)
	(error "Bad tagged -- TYPE-TAG" data)
))

(define (contents data)
    (if (pair? data)
	(cdr data)
	(error "Bad tagged -- CONTENTS" data)
))



(define (square x) (* x x))


;;直交座標系
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

    ;;インターフェース
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


;;極座標系
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

    ;;インターフェース
    (define (tag x) (attach-tag 'polar x))

    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)

    (put 'make-from-real-imag
	 'polar
	 (lambda (x y) (tag (make-from-real-imag x y)) ) )

    (put 'make-from-mag-ang
	 'polar
	 (lambda (r a) (tag (make-from-mag-ang r a)) ) )

'done)



;;演算手続き
(define (apply-generic op . args)
    (let ((tags (map type-tag args) ))
        (let ((proc (get op tags) ))
	    (if proc
		(apply proc (map contents args))
		(error "Not method for these types -- APPLY-GENERIC"
		       (list op tags))
))))

;;初期化
(install-rectangular-package)
(install-polar-package)


;;汎用選択子
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;;構成子
(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y) )

(define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a) )

;;算術演算
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
(define (print-complex z)
    (if (= (real-part z) 0)
	(display "")
	(display (real-part z)) )
    (cond ((= (imag-part z) 0) )
	  ((= (imag-part z) 1) (display "+i"))
	  ((= (imag-part z) -1) (display "-i"))
	  (else
              (if (> (imag-part z) 0)
		  (display "+")
	      )
	      (display (imag-part z))
	      (display "i") )
    )
    (newline)
)