; #lang scheme

; 手続きを n回適用する手続き repeated の定義
; (repeated f n) -> (f (f (...(f x)...))


; 前提関数
(define (square x) (* x x))


; repeated
(define (repeated f n)
  (lambda (x)
    (let loop ((i 1)
               (x x))
      (if (> i n)
          x
          (loop (+ i 1) (f x))))))
 
; run
((repeated square 2) 5)
; -> 625