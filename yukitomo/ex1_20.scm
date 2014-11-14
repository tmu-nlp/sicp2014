;ex1_20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;GCD (206,40)
;= GCD (40,6)
;= GCD (6,4)
;= GCD (4,2)
;= GCD (2,0)
;= 2


;normal-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 6 (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40))(remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40))  (remainder 40 (remainder 206 40)))))


;applicative-order evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2