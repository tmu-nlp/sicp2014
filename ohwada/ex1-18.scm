; 対数的ステップ数の反復的乗算プロセス

(define (double a) (* a 2))
(define (halve a) (/ a 2))
(define (even? n)
  (= (remainder n 2) 0))


(define (* b n)
  (fast-*-iter b n 0))

(define (fast-*-iter b n a)
  (cond ((= n 0) a)
        ((= n 1) (+ a b))
        ((even? n) (fast-*-iter (double b) (halve n) a))
        (else (fast-*-iter b (- n 1) (+ a b)))))


(print (* 3 4))     ; 12
(print (* 9 12))    ; 108
(print (* 13 23))   ; 299
