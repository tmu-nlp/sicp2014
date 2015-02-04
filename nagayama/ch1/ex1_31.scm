; #lang scheme

; 総積の一般形 product の設計.
; 線形再帰プロセスと反復再帰プロセスの両方を作る.
; factorial の定義
; pi の近似値


; 前提関数
(define (even? n) (= (remainder (round n) 2) 0))
(define (inc x) (+ x 1))
(define (identity x) x)


; 総積の一般形(recursive)
(define (product-re term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-re term (next a) next b))))

; 総積の一般形(iterative)
(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


;; factorial
(define (factorial x)
  (product-rew identity 1 inc x))


;; pi-culc
(define (pi-culc x)
  (define (pi-term x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  (* 4 (product-it pi-term 1 inc x)))
               

; run
(factorial 5)　; -> 120
(pi-culc 1000)　; -> 3.1431...

