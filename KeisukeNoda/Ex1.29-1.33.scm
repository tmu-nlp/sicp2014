;;;; Exercise 1.29 ;;;;

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a ) next b))))

(define (inc n) (+ n 1))
(define (cube n) (* n n n))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))  ; h  = (b - a)/n
  (define (yk k)            ; yk = f(a + kh)
    (define m
      (cond ((= k 0) 1)
	    ((= k n) 1)
	    ((even? k) 2)
	    (else 4)))
    (* m (f (+ a (* k h)))))
  (* (/ h 3) (sum yk 0 inc n)))

(simpson cube 0 1.0 100)
(simpson cube 0 1.0 100000)

;; gosh> 0.24999999999999992
;; gosh> 0.24999999999999464

(integral cube 0 1 0.01)
(integral cube 0 1 0.00001)

;; gosh> 0.24998750000000042
;; gosh> 0.24999999998662864


;;;; Exercise 1.30 ;;;;

;; 反復sum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (sum-integers a b)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (sum identity a inc b))

(sum-integers 1 10)
;; gosh> 55


;; 比較のために再帰のsum
(define (re-sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (re-sum term (next a) next b))))

(define (re-sum-integers a b)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (re-sum identity a inc b))

(re-sum-integers 1 10)
;; gosh> 55

;;;; Exercise 1.31 ;;;;

;; (a) 与えられた範囲の点での関数値の積を返すproductを用いたfactorial ;;

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; John Wallisの式
(define (wallis n)
  (define (square x) (* x x))
  (define (next x) (+ x 1))
  (define (term x)
    (/ (* (* x 2) (* (+ x 1) 2))
       (square (+ (* x 2) 1))))
  (product term 1.0 next n))

(wallis 10)
(wallis 1000)
(wallis 100000)

;; gosh> 0.803446235073297
;; gosh> 0.7855943412734705
;; gosh> 0.7854001268753947

;; (b) 再帰だったので、反復プロセス 

(define (re-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* a result))))
  (iter a 1))

(define (factorial n)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (product identity 1 inc n))

(define (re-factorial n)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (re-product identity 1 inc n))

(factorial 6)
(re-factorial 6)

;; gosh> 720
;; gosh> 720


;;;; Exercise 1.32 ;;;;


;; (a) 再帰のaccumulate ;;
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))1

(define (identity x) x)
(define (inc x) (+ x 1))

(accumulate * 1 identity 1 inc 5)
;; gosh> 120
;; 1 * 2 * 3 * 4 * 5 = 120

(accumulate + 0 identity 1 inc 5)
;; gosh> 15
;; 1 + 2 + 3 + 4 + 5 = 15



;; (b) 反復のaccumulate ;;
(define (re-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(re-accumulate * 1 identity 1 inc 5)
;; gosh> 120
;; 1 * 2 * 3 * 4 * 5 = 120
(re-accumulate + 0 identity 1 inc 5)
;; gosh> 15 
;; 1 + 2 + 3 + 4 + 5 = 15


;;;; Exercise 1.33 ;;;;

;; filtered-accumulate手続き
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate filter combiner null-value term (next a) next b))))

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

;; (a) 区間a, bの素数の二乗の和 ;;
(define (sum-square-prime a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; (b) nと互いに素で、nより小さい正の整数の積 ;;
(define (product-gcd n)
  (define (gcd-filter x) (= (gcd x n) 1))
  (filtered-accumulate gcd-filter * 1 identity 1 inc n))


(sum-square-prime 1 10)
;; gosh> 87
;; 2*2 + 3*3 + 5*5 + 7*7 = 87

(product-gcd 5)
(product-gcd 6)
(product-gcd 7)

;; gosh> 24
;; 2 * 3 * 4
;; gosh> 5
;; 5のみ
;; gosh> 720
;; 2 * 3 * 4 * 5 * 6
