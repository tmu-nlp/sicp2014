;;;; Exercise 1.16 ;;;;

(define (square x)  ;; 二乗計算
  (* x x))

;; a * b^n
;; a * b^0 = 1               (n = 0)
;; a * b^n = a * (b ^ n/2)^2 (n = 2m)   (m = 1,2,,,)
;; a * b^n = a * b * b^(n-1) (n = 2m-1) (m = 1,2,,,)

;; 指数nと底bの他にもう一つの状態変数aを用意し計算のステップ数を少なくする

(define (fast-expt-i a b n)
  (cond ((= n 0) a)
        ((even? n)
          (fast-expt-i a (square b) (/ n 2)))
        (else
          (fast-expt-i (* a b) b (- n 1)))))

;; b^n
;; b^0 = 1           (n = 0)
;; b^n = (b ^ n/2)^2 (n = 2m)     (m = 1,2,...)
;; b^n = b * b^(n-1) (n = 2m - 1) (m = 1,2,...)

(define (fast-expt b n)
  (fast-expt-i 1 b n))

(fast-expt 2 6)

;;;; Exercise 1.17 ;;;;

(define (double x) (+ x x))
(define (halve x) (/ x 2))


;; a*b
;; a*b = 0             (b = 0)
;; a*b = 2a * b/2      (b = 2n)     (n = 1,2,...)
;; a*b = a + a * (b-1) (b = 2n - 1) (n = 1,2,...)

(define (multiply a b)
  (cond ((= b 0) 0)
        ((even? b) (multiply (double a) (halve b)))
        (else (+ a (multiply a (- b 1))))))

(multiply 2 6)

;;;; Exercise 1.18 ;;;;

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multiply-i n a b)
  (cond ((= b 0) n)
        ((even? b) (multiply-i n (double a) (halve b)))
        (else (multiply-i (+ n a) a (- b 1)))))

(define (multiply2 a b)
  (multiply-i 0 a b))

(multiply2 2 6)

;;;; Exercise 1.19 ;;;;

(define (Fib n)
    (Fib-iter 1 0 0 1 n))

(define (Fib-iter a b p q count)
    (cond
        ((= count 0) b)
        ((even? count)
          (Fib-iter a
		    b
		    (+ (* p p) (* q q))
		    (+ (* 2 p q) (* q q))
		    (/ count 2)))
        (else (Fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))


#|----

a ← a + b, b ← a への変換 T を以下の行列式に置き換えて計算する.

|#

;;;; Exercise 1.20 ;;;;

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(print (gcd 206 40))

;; gosh> 2



