;#lang racket
;;;;;;1_21.scm
(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (real-time-clock) start-time))))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (next n)
  (cond ((even? n) (+ n 1))
        (else (+ n 2))))

(define (search-for-prime n m)
  (cond ((< n m)
         (timed-prime-test n)
         (search-for-prime (next n) m))
        (else 
          (display "\nall searched"))))

(search-for-prime 1000 1100)
;計算が早すぎるので、測った時間は全て０になった
;解決の方法の一つとしては、例えば1000個の整数を素数かどうか判断した後
;平均時間を計算する->1_22_1.scm
