(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(print (A 1 10))  ; 1024
(print (A 2 4))   ; 65536
(print (A 3 3))   ; 65536


(define (f n) (A 0 n))
(f 0)   (A 0 0)    0
(f 1)   (A 0 1)    2
(f 2)   (A 0 2)    4
(f 3)   (A 0 3)    6

; (f, n) = 2n


(define (g n) (A 1 n))
(f 0)   (A 1 0)    0
(f 1)   (A 1 1)    2
(f 2)   (A 1 2)    (A (- 1 1) (A 1 (- 2 1)))
                   (A 0 (A 1 1))
                   (A 0 2)
                   4
(f 3)   (A 1 3)    (A (- 1 1) (A 1 (- 3 1)))
                   (A 0 (A 1 2))
                   (A 0 4)
                   8
(f 4)   (A 1 4)    (A (- 1 1) (A 1 (- 4 1)))
                   (A 0 (A 1 3))
                   (A 0 8) 
                   16 

; (g, n) = 2^n


(define (h n) (A 2 n))
(f 0)   (A 2 0)    0
(f 1)   (A 2 1)    2
(f 2)   (A 2 2)    (A (- 2 1) (A 2 (- 2 1)))
                   (A 1 (A 2 1))
                   (A 1 2)
                   4
(f 3)   (A 2 3)    (A (- 2 1) (A 2 (- 3 1)))
                   (A 1 (A 2 2))
                   (A 1 4)
                   16
(f 4)   (A 2 4)    (A (- 2 1) (A 2 (- 4 1)))
                   (A 1 (A 2 3))
                   (A 1 16)
                   65536
(f 5)   (A 2 5)    (A (- 2 1) (A 2 (- 5 1)))
                   (A 1 (A 2 4))
                   (A 1 65536)
                   2 ** 65536 


; (h, n) は n = 0 のときは 0
;           n = 1 のときは 2
;           n > 1 のときは 2 の (h, n-1) 乗 (2↑↑n) 
