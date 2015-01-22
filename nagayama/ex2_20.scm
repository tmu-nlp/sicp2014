#lang scheme

; 任意の個数の引数を取る手続きの定義
; same-parity : 1つ以上の引数をとり,先頭と偶奇が同じ数のみのリストを返す

(define (same-parity x . y)
  (define (iter s y d)
    (if (null? y)
        s
        (if (= d (remainder (car y) 2))
            (iter (append s (list (car y))) (cdr y) d)
            (iter s (cdr y) d))))
  (let ((d (remainder x 2))
        (s (list x)))
    (iter s y d)))


; run
(same-parity 1 2 3 4 5 6 7)
(same-parity 3 1 4 1 5 9 2)
      
