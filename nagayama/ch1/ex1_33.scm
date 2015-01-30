; #lang scheme

; filtered-accumulate の設計をする.
; 素数の二乗和, coprimeの積の計算をする.


; 前提関数
(define (even? n) (= (remainder (round n) 2) 0))
(define (inc x) (+ x 1))
(define (identity x) x)
(define (square x) (* x x))
(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond
     ((> (square test-divisor) n) n)
     ((divides? test-divisor n) test-divisor)
     (else
      (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (zero? (modulo b a)))
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

; 前提関数 ここまで


;; accumulate (liner)
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate combiner
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b
                                     filter))))

;; accumulate (recursive)
(define (filtered-accumulate-re combiner null-value term a next b filter)
  (define (iter combiner a filter result)
    (if (> a b)
        result
        (iter combiner
              (next a)
              filter
              (combiner result
                        (if (filter a)
                            (term a)
                            null-value)))))
  (iter combiner a filter null-value))

;; a
; aとbの間の素数の二乗和
(define (sum-square-prime a b)
  (filtered-accumulate + 0 square a inc b prime?))

;; b
; nと互いに素なn未満の自然数の積
(define (product-coprime n)
  (define (coprime? x) (= (gcd x n) 1))
  (filtered-accumulate-re * 1 identity 1 inc n coprime?))


; run
(sum-square-prime 1 10)
; (+ 4 9 25 49) = 87

(product-coprime 10)
; (* 1 3 7 9) = 189
