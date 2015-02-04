; #lang scheme

; sin(x) を再帰手続きで求める際のオーダー.

; 前提関数 
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))


; p の適用回数の counter を追加したsine
(define (sine angle)
  (define (iter angle count)
    (if (not (> (abs angle) 0.1))
        (begin (display count)
               (newline)
               angle)
        (p (iter (/ angle 3.0) (+ count 1)))))
  (iter angle 0))

; (sine 12.15)
; -0.39980345741334

; a (sine 12.15) を実行したときの p の適用回数
; (sine 12.15)
; 5
; -0.39980345741334
; より、5 回

; b 各オーダー
; 記憶域 : log(n)
; ステップ : log(n)