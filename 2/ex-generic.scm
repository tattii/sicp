(load "generic.scm")


;;integer
(define n1 (make-integer 5))
(print n1)

(define n2 (make-integer 3))
(print n2)

(print (add n1 n2))
(print (div n1 n2))


;;rational
(define q1 (make-rational 3 2))
(print q1)

(define q2 (make-rational 9 6))
(print q2)

(print (div q1 q2))


;;real
(define r1 (make-real 3.7))
(print r1)

(define r2 (make-real 2.7))
(print r2)

(define r3 (make-real 1))
(print r3)

(print (sub r1 r2))


;;complex
(define z1 (make-complex-from-real-imag 1 1))
(print z1)

(define z2 (make-complex-from-real-imag 1 -1))
(print z2)

(print (add z1 z2))
(print (sub z1 z2))
(print (mul z1 z2))

;;integer + complex
(print (add n1 z1))

;;rational + complex
(print (add q1 z1))

;;rational + integer
(print (add q1 n1))
(print (div q1 n1))

;;普通の演算子のようにも使える
(print (add 11 4))
(print (mul 2.7 4))