#!/usr/bin/gosh
(use srfi-19)
(use srfi-27)
(load "./ex01_22.scm")

(define true #t)
(define false #f)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (and (fast-prime? n 10)
       (report-prime (time-difference (current-time) start-time))))

(search (expt 10 3))
(search (expt 10 6))
(search (expt 10 9))
(search (expt 10 12))
(search (expt 10 15))

;; Fermat テストは log n の増加の程度なので以下の数をテストすると 1:2:3:4:5 の比率になると予想できる.
;; 実際は 1:1.7:2.6:8:10 程度であった. これは log n の増加の程度に比例した時間になっていない.
