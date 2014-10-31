(define (A x y)
        (cond ((= x 0) (* 2 y))
              ((= y 0) 0)
              ((= y 1) 2)
              (else (A (- x 1) (A x (- y 1))))))

; (define (A x y)
;         (cond ((= x 0) (+ y 1))
;               ((= y 0) (A (- x 1) 1))
;               (else (A (- x 1) (A x (- y 1))))))


; (A 1 10)
; = 1024 

; (A 2 4)
; = 65536

; (A 3 3)
; = 65536


(define (f n) (A 0 n))
; 2n を返す関数

(define (g n) (A 1 n))
; 2^n を返す関数

(define (h n) (A 2 n))
; 2↑↑n を返す関数

