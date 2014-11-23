; 下で使うので square と even? を定義

(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))

;-----------------------------
; fast-prime? 手続き

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (use srfi-27)
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;-------------------------------
; fast-expt を用いた expmod 計算手続き

(define (expmod base exp m)
   (remainder (fast-expt base exp) m))
 
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(print (fast-prime? 1000037 100))   ; 非常に時間がかかる


; expmod 計算手続き

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(print (fast-prime? 1000037 100))  ; すぐ結果が出る

; 上はsquare を全て評価してから余りを求めるが、下はその都度
; 余りを求めてから square を評価するので、下の方がはるかに早い


