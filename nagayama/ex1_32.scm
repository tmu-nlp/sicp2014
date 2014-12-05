; #lang scheme

; accumulate(集積)
; 線形プロセスと再帰プロセスの両方を作成する.

; 前提関数
(define (inc x) (+ x 1))
(define (identity x) x)


;; accumulate(recursive)
(define (accumulate-re combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate-re combiner null-value term (next a) next b))))

;; accumulata(iterative)
(define (accumulate-it combiner null-value term a next b)
  (define (iter combiner a result)
    (if (> a b)
        result
        (iter combiner (next a) (combiner result (term a)))))
  (iter combiner a null-value))


; 以下、実行テスト

;; 奇数和 (iterative
(define (odd-square x)
  (define (odd x) (- (* 2 x) 1))
  (accumulate-it + 0 odd 1 inc x))

;; 階乗 (recursive)
(define (factorial x)
  (accumulate-re * 1 identity 1 inc x))
               
; run
(odd-square 5)
(factorial 5)
