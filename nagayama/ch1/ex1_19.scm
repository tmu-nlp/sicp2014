; #lang scheme

; フィボナッチ数を対数ステップ数で求める手続き.
; p'とq'を求めて手続きを完成させる.

; 前提関数
(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))


; fib
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a 
                   b
                   (+ (square p) (square q)) ; p'
                   (+ (* 2 p q) (square q))  ; q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;
; p', q' の導出
; 
; fib を 2 回適用した式を整理する。
;
;    fib ( fib (a b p q) )
;  = fib ( (bq+aq+ap) (bp+aq) p q )
;  = fib ( ((bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p)
;          ((bp+aq)p + (bq+aq+ap)q)
;          p
;          q )
;  = fib ( ( b * (      2pq + qq) + a * (pp + 2pq + 2qq) )
;          ( b * ( pp +       qq) + a * (     2pq +  qq) )
;          p
;          q)
;
; ここで,
; 
;  fib(a b p' q') = fib(fib(a b p q))
; 
; となる p', q' を考えると, 第1項, 第2項をそれぞれ比較して
;
;  bq'+ aq'+ap' = b   * (      2pq + qq) + a * (pp + 2pq + 2qq)
;  bp'+     aq' = b   * ( pp +       qq) + a * (     2pq +  qq)
;
; これら 2 式から a, b の係数に注目して
; 
;  p' =  pp + qq
;  q' = 2pq + qq
;
; となることがわかる.
; 
; 