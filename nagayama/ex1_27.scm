; #lang scheme

; 前提関数
(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

; base^exp (mod m)
(define (expmod base exp m)
  (cond ((= exp 0)
         1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
         (else
         (remainder (* base (expmod base (- exp 1 ) m))
                    m)))) 

; fermat-test
(define (fermat-test n a)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it a))

; 高速素数判定 (n未満のすべての自然数について回す)
(define (fast-prime? n a)
  (cond ((<= n a) true)
        ((fermat-test n a) (fast-prime? n (+ a 1)))
        (else false)))


; 測定 runtime が無いので、代わりに current-milliseconds を用いた
(define (runtime) (current-milliseconds))

(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (when (fast-prime? n 1)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

; from から n 個の整数のうち、奇数についてだけ調べる
(define (search-for-prime n)
  (cond ((< n 0) (newline)　'done )
        (else (timed-prime-test (CarmichelNum n))
              (search-for-prime (- n 1)))))

; Carmichel numbers
(define (CarmichelNum serial)
  (cond ((= serial 1) 561)
        ((= serial 2) 1105)
        ((= serial 3) 1729)
        ((= serial 4) 2465)
        ((= serial 5) 2821)
        ((= serial 6) 6601)))
  
; run
(search-for-prime 6)

; 6601 *** 32
; 2821 *** 7
; 2465 *** 6
; 1729 *** 4
; 1105 *** 3
; 561 *** 1
;
; 6つ全て素数だと判定された。
;