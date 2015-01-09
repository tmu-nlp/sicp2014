; #lang scheme

; 平均減衰を用いて n乗根 を求めるとき, 
; 平均減衰を何回適用する必要があるか調べる.
; その結果をもとに, 平均減衰の回数を
; 引数に取らない n乗根を求める手続きを実装する.


; 前提関数
(define (average x y) (/ (+ x y) 2))



; 平均減衰 : average-damp
(define (average-damp f)
  (lambda (x) (average x (f x))))

; 繰り返し : repeated
(define (repeated f n)
  (lambda (x)
    (let loop ((i 1)
               (x x))
      (if (> i n)
          x
          (loop (+ i 1) (f x))))))

; 不動点調査 : fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess count)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (if (> count 65535)  ;; ループが 65535回を超えたら不定と判断
              #f
              (try next (+ count 1))))))
  (try first-guess 1))

; (n-root x n k)  :  x の n乗根 を 平均減衰 k回 で求める.
(define (n-root x n k)
  (fixed-point
   ((repeated average-damp k)
    (lambda (y)
      (/ x (expt y (- n 1)))))
    1.0))

; 各k について何乗根まで計算できるか調査
(define (test kmax)
  (define (k-test n k)
    (if (n-root (expt 3 n) n k)
        (k-test (+ n 1) k)
        (- n 1)))
  (display "k --> n")
  (newline)
  (let loop ((i 0))
    (if (> i kmax)
        (display "fin")
        (begin (display i)
               (display " --> ")
               (print (k-test 1 i))
               (newline)
               (loop (+ i 1))))))
  
; 実行結果
; > (loop-test 5)
; k --> n
; 0 --> 1
; 1 --> 3
; 2 --> 7
; 3 --> 15
; 4 --> 31
; 5 --> 63
; fin


; よって, k = ( log(n)/log(2) の切り捨て )
; ならば不定が発生しないと予想される.
; ここまでの結果を踏まえて n乗根を求める手続きを定義する.


(define (new-n-root x n)
  (fixed-point
   ((repeated average-damp (floor (/ (log x) (log 2))))
    (lambda (y)
      (/ x (expt y (- n 1)))))
    1.0))


; 実行結果
; > (new-n-root 32 5)
; 1.999952697065906
; 
; > (new-n-root (expt 6 6) 6)
; 5.946616828091337
; 




