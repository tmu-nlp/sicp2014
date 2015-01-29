; #lang scheme

; 前提関数
(define (square x) (* x x))

; 約数探査
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

; 素数判定
(define (prime? n)
  (= n (smallest-divisor n)))

; 測定 runtime が無いので、代わりに current-milliseconds を用いた
(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

; from から n 個の整数のうち、奇数についてだけ調べる
(define (search-for-prime from n)
  (cond ((< n 0) (newline) 'done )
        ((even? from) (search-for-prime (+ from 1) (- n 1)))
        (else (timed-prime-test from)
              (search-for-prime (+ from 2) (- n 2)))))

; next
(define (next x)
  (if (= x 2) 3 (+ x 2)))
        

; run
; (search-for-prime          1000 100) ;   1009,   1013,   1019 ; 
; (search-for-prime         10000 100) ;  10007,  10009,  10037 ; 
; (search-for-prime        100000 100) ; 100003, 100019, 100043 ; 
; (search-for-prime  100000000000 100) ;  1..03,  1..19,  1..57 ; 20 ms
; (search-for-prime 1000000000000 100) ;  1..39,  1..61,  1..63 ; 62 ms
;
; 計算時間は ex1.22 に比べて 1/2 ~ 2/3 になった。
; 2 以外の偶数についての処理を飛ばしたため、処理時間が半分近くになったと考えられる。
;
