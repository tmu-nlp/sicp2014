; #lang scheme

; sum 手続きを反復再帰で書き直す.


; 前提関数
(define (even? n) (= (remainder (round n) 2) 0))
(define (inc x) (+ x 1))

; 総和の一般形 (recursive)
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))



; 以下, 実行テスト

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
  (* (/ h 3)
     (sum simp-term 0 inc n)))

; y
(define (cube x) (* x x x))

; run
(simpson cube 0 1 100)
; -> 1/4

;(simpson sin 0 (/ pi 2) 100)
; -> 1.00

