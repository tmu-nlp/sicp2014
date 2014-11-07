;ex1_20.scm

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;normal-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40) (remainder 6 (remainder 15 6)))
(gcd 3 (remainder 6 (remainder 15 6)))
(gcd (remainder 6 (remainder 15 6)) (remainder 3 (remainder 6 (remainder 15 6))))
(gcd (remainder 6 3) (remainder 3 (remainder 6 (remainder 15 6))))
(gcd 0 (remainder 3 (remainder 6 (remainder 15 6))))
(remainder 3 (remainder 6 (remainder 15 6)))
(remainder 3 (remainder 6 2))
(remainder 3 0)
3


;applicative-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)

