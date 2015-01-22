;;;; Exercise 1.40 ;;;;

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)   ;; 微分
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;; x^3 - x = 0
;; => x(x^2 -1) = 0
;; => x = 0, 1, -1

(newtons-method (cubic 0 -1 0) 2.0)
;; gosh> 1.0000000000000002

(newtons-method (cubic 0 -1 0) -2.0) 
;; gosh> -1.0000000000000002

(newtons-method (cubic 0 -1 0) 0.2) 
;; gosh> 9.019835590297049e-25

;;;; Exercise 1.41 ;;;;

;; 引数として一引数の手続きをとり、
;; 受け取った手続きを二回作用させる
;; 手続きを返す手続きdoubleの実装

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

((double inc) 1)
;; gosh> 3
;; (double inc;; => (lambda (x) (inc (inc x)))

(((double (double double)) inc) 5)
;; gosh> 21

;;;; Exercise 1.42 ;;;;

;; 一引数の関数fとgを用いて
;; gの後のfの合成関数を定義

(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))

(define (square x) (* x x))

((compose square inc) 6)

;; (* (+ 6 1) (+ 6 1))
;; gosh> 49

;;;; Exercise 1.43 ;;;;

;; fをn回作用させる手続き

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i g)
    (if (= i n)
        g
        (iter (+ i 1) (compose f g))))
  (iter 1 f))

(define (square x) (* x x))
(define (inc x) (+ x 1))

((repeated square 2) 5)
;; gosh> 625

((repeated inc 10) 5)
;; gosh> 15

;;;; Exercise 1.44 ;;;;


;; n重平滑化関数の手続き

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i g)
    (if (= i n)
        g
        (iter (+ i 1) (compose f g))))
  (iter 1 f))

(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)   (repeated (smooth f) n))


;;;; Exercise 1.45 ;;;;

;; n乗根をm回の平均緩和を使って解いて収束を見る実験

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2))
(define (close-enough? x y) (< (abs (- x y)) 0.001))

;; 平均減衰 average-damp
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; 繰り返し repeated
(define (repeated f n)
  (lambda (x)
    (let loop ((i 1)
               (x x))
      (if (> i n)
          x
          (loop (+ i 1) (f x))))))


(define (n-fold-average-damp n) 
  (repeated average-damp n))

(define (n-root-exp n x m)
  (fixed-point ((n-fold-average-damp m)
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(n-root-exp 2 2 0) ;; &#9747;
(n-root-exp 2 2 1)
(n-root-exp 3 2 1)
(n-root-exp 4 2 1) ;; &#9747;
(n-root-exp 4 2 2)
(n-root-exp 5 2 2)
(n-root-exp 6 2 2)
(n-root-exp 7 2 2)
(n-root-exp 8 2 2) ;; &#9747;
(n-root-exp 8 2 3)

;; ここから、必要なm回は、n = 2^iとしたときの、
;; i以下の最も大きな整数
;; と推測できる

(define (n-root n x)
  (define (damp-count m)
    (if (< m 2)
        0
        (+ 1 (damp-count (/ m 2)))))
  (fixed-point ((n-fold-average-dump (damp-count n)) 
                (lambda (y) (/ x (expt y (- n 1)))))))


;;;; Exercise 1.46 ;;;;

;; iterative-improve は、「引数として予測値をとり、予測値が十分良好になるまで改良を繰り返す手続き」を値として返す手続き ;;
(define (iterative-improve good-enough? improve) ;; <<- 予測値が十分良好であるかを調べる手続き
  (define (check guess)
    (let ((next-guess (improve guess)))
      (if (good-enough? guess next-guess)
          next-guess
          (check next-guess))))
  (lambda (initial-guess)                         ;; <<- 予測値を入れて、改良を繰り返す手続き
    (check initial-guess)))


(define (average x y)
  (/ (+ x y) 2))
 
(define (square x) (* x x))
 
;; sqrtの手続き
;; good-enough? >> 予測値が十分であるかを調べる手続き
;; improve      >> 予測値を改良する 

(define (sqrt x)
  (define (good-enough? guess next-guess)        ;; 精度チェック
    (< (abs (- (square next-guess) x)) 0.001))
  (define (improve guess)                        ;; 改良手続き
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) x))  ;; 反復改良法を用いる.

(sqrt 9.0)
;; gosh> 3.00009155413138



;; fixed-pointの手続き
;; close-enough? >> 予測値が十分であるかを調べる手続き
;; improve       >> 予測値を改良する 

(define tolerance 0.00001)
 
(define (fixed-point f first-guess)
  (define (close-enough? guess next-guess)                  ;; 精度チェック
    (< (abs (- guess next-guess)) tolerance))
  (define (improve guess)                                   ;; 改良手続き
    (f guess))
  ((iterative-improve close-enough? improve) first-guess))  ;; 引数として予測値をとり、予測値が改良されるまで繰り返す.

(define (average-damp f)
  (lambda (x) (average x (f x))))
 
(define (fixed-point-sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(fixed-point-sqrt 9.0)
;; gosh> 3.0
