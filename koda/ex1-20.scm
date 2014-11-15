(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(print (gcd 206 40))
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)

; 適用順評価ではremainderの使用回数は4

;(if (= 40 0) 206 (gcd 40 (remainder 206 40)))
;GCD1 = (remainder 206 40) = 6 
;(gcd 40 GCD1)
; =(if (= GCD1 0) 40 (gcd GCD1 (remainder 40 GCD1)))
;GCD2 = (remainder 40 GCD1) = 4
;(gcd GCD1 GCD2)
; =(if (= GCD2 0) GCD1 (gcd GCD2 (remainder GCD1 GCD2)))
;GCD3 = (remainder GCD1 GCD2) = 2
;(gcd GCD2 GCD3)
; =(if (= GCD3 0) GCD2 (gcd GCD3 (remainder GCD2 GCD3)))
;GCD4 = (remainder GCD2 GCD3) = 0
;(gcd GCD3 GCD4)
; =(if (= GCD4 0) GCD3 (gcd GCD4 (remainder GCD3 GCD4)))
;
;GCD1は1回,GCD2は2回,GCD3は4回,GCD4は7回
;最後のif文の判定でremainderを4回使うため
; 正規順評価ではremainderの使用回数は18
