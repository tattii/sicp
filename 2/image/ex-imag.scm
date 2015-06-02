(load "imag2.scm")

(define z1 (make-from-real-imag 1 1))

(define z2 (make-from-real-imag 1 -1))

(print-complex z1)
;;1+i

(print-complex z2)
;;1-i

(print-complex (add-complex z1 z2))
;;2

(print-complex (sub-complex z1 z2))
;;2i

(print-complex (mul-complex z1 z2))
;;2

(print-complex (div-complex z1 z2))
;;i
