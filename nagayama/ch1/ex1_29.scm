; #lang scheme

; シンプソンの公式を用いた定積分の手続き.


; 前提関数
(define (even? n) (= (remainder (round n) 2) 0))
(define (inc x) (+ x 1))

; 総和の一般形
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; simpson
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simp-coef x)
    (cond ((or (= x 0) (= x n)) 1)
          ((even? x) 2)
          (else 4)))
  (define (simp-term k)
    (* (simp-coef k)
       (f (+ a (* k h)))))
  (* (/ h 3) (sum simp-term 0 inc n)))



; 以下、実行テスト

; 具体的な f(x)
(define (cube x) (* x x x))

; run
(simpson cube 0 1 100)
; -> 1/4

;(simpson sin 0 (/ pi 2) 100)
; -> 1.00
