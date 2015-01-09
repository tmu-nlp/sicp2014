#lang scheme

; 素因数分解の一意性より題意は自明

(define (even? n) (= (remainder (round n) 2) 0))


(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (define (iter z count)
    (if (even? z)
        (iter (/ z 2) (+ count 1))
        count))
  (iter z 0))

(define (cdr z)
  (define (iter z)
    (if (even? z)
        (iter (/ z 2))
        (/ (log z) (log 3))))
  (iter z))

; run
(car (cons 2 4))
(cdr (cons 5 3))
