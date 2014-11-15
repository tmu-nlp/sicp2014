(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


; 正規順序
; if では分岐を判断するのに評価するがその後の gcd 
; には (remainder a b) をそのまま渡す。
(gcd 206 40)
(if (= 40 0))  ; #f
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0))  ; #f
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0))  ; #f
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0))  ; #f
(gcd (remainder (remainder 206 40) (remainder 40 (remainder (206 40)))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0))  ; #t

; remainder は18回実行される（下2行より）


; 作用的順序
(gcd 206 40)
(if (= 40 0)) 
(gcd 40 (remainder 206 40))
(gcd 40 6)
(if (= 6 0))
(gcd 6 (remainder 40 6))
(gcd 6 4)
(if (= 4 0))
(gcd 4 (remainder 6 4))
(gcd 4 2)
(if (= 2 0))
(gcd 2 (remainder 4 2))
(gcd 2 0)
(if (= 0 0))
2

; remainder は4回実行される

