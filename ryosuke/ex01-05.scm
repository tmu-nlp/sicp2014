;;with an applicative-order evaluation, this evaluation never finish.
;;wit an normal-order evaluation, interpreter output '0'.

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(print (test 0 (p)))
