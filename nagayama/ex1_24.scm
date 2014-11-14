#lang scheme

; 前提関数
(define (square x) (* x x))

; 偶奇判定
(define (even? n) (= (remainder n 2) 0))

; base^exp (mod m)
(define (expmod base exp m)
  (cond ((= exp 0)
         1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
         (else
         (remainder (* base (expmod base (- exp 1 ) m))
                    m)))) 

; fermat-test
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; 高速素数判定 (times回挑戦する)
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


; 測定 runtime が無いので、代わりに current-milliseconds を用いた
(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (when (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

; from から n 個の整数のうち、奇数についてだけ調べる
(define (search-for-prime from n)
  (cond ((< n 0) (newline)　'done )
        ((even? from) (search-for-prime (+ from 1) (- n 1)))
        (else (timed-prime-test from)
              (search-for-prime (+ from 2) (- n 2)))))
  
; run
; (search-for-prime          1000 100) ;    1009,    1013,    1019 ; 
; (search-for-prime         10000 100) ;   10007,   10009,   10037 ; 
; (search-for-prime        100000 100) ;  100003,  100019,  100043 ; 
; (search-for-prime     1000000000 1000) ; 1000003, 1000033, 1000037 ; 
; (search-for-prime  100000000000 100) ;   1..03,   1..19,   1..57 ;   ms
; (search-for-prime 1000000000000 100) ;   1..39,   1..61,   1..63 ;  ms
;
; 11桁ではオーバーフローしてしまった。
; 
