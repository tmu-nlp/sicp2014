; prime? 手続きを定義
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (define (square x) (* x x))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;----------------------------------------

; Euclid アルゴリズムで最大公約数を求める手続き gcd を定義
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;----------------------------------------

; filtered-accumulate
(define (filtered-accumulate filter-func combiner null-value term a next b)
  (define (filter x)
    (if (filter-func x) (term x) null-value))
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (combiner result (filter x)))))
  (iter a null-value))


(define (inc x) (+ x 1))

; filtered-accumulate を用いて素数の二乗の和を計算する手続き
(define (sum-square-prime a b)
  (define (combiner x y) (+ x y))
  (define (square x) (* x x))
  (filtered-accumulate prime? combiner 0 square a inc b))


; テスト
(print (sum-square-prime 2 9)) ; 2^2+3^2+5^2+7^2=87


; filtered-accumulate を用いて n と互いに素かつ n より小さい整数の積を計算する手続き
(define (coprime-product a n)
  (define (combiner x y) (* x y))
  (define (identity x) x)
  (define (filter-func x)
     (= (gcd x n) 1))
  (filtered-accumulate filter-func combiner 1 identity a inc n))


; テスト
(print (coprime-product 1 5)) ; 1*2*3*4=24
(print (coprime-product 1 6)) ; 1*5=5
