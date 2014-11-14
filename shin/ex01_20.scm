#!/usr/bin/gosh


(define (gcd a b c)
    (if (= b 0)
        a
        (gcd b (remainder a b) #?=(+ c 1))))

(print (gcd 206 40 0))
;; => 2


; remainder 使用累計 0 回
;(if (= 40 0) ;; => #f
;    206
;    (gcd 40 (remainder 206 40)))

; remainder 使用累計 1 回
;(if (= 6 0) ;; => #f
;    40
;    (gcd 6 (remainder 40 6)))

; remainder 使用累計 2 回
;(if (= 4 0) ;; => #f
;    6
;    (gcd 4 (remainder 6 4)))

; remainder 使用累計 3 回
;(if (= 2 0) ;; => #f
;    4
;    (gcd 2 (remainder 4 2)))

; remainder 使用累計 4 回
;(if (= 0 0) ;; => #t
;    2
;    (gcd 0 (remainder 2 0)))