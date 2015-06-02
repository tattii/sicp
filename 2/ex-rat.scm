(load "rat.scm")

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

(print-rat one-half) ;;1/2

(print-rat one-third) ;;1/3

(print-rat (add-rat one-half one-third)) ;;5/6

(print-rat (sub-rat one-half one-third)) ;;1/6

(print-rat (mul-rat one-half one-third)) ;;1/6

(print-rat (div-rat one-half one-third)) ;;3/2

(display (equal-rat? (add-rat (add-rat one-half one-third) (make-rat 1 6)) 
		     (make-rat 1 1)) )
;;#t


(newline)