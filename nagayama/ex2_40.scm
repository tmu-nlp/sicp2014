#lang scheme

; unique-pairs の定義
; それを用いて prime-sum-pairs を簡単にする


(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))


; 以下, 実行テスト

(define (enumerate-interval low high)
    (if (> low high)
        null
        (cons low (enumerate-interval (+ low 1) high))))

; 集積
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (car (cdr pair)))))
(define (make-pair-sum pair)
  (list (car pair) (car (cdr pair)) (+ (car pair) (car (cdr pair)))))

(define (square x) (* x x))
(define (prime? n) (= n (smallest-divisor n)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))



; run
(prime-sum-pairs 6)