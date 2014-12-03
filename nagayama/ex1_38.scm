; #lang scheme

; 連分数 から e (Napier's) の近似値を求める.


; 前提関数
(define (close-enough? x y) (< (abs (- x y)) 0.001))


; D(i)
(define (D i)
  (cond ((= (modulo i 3) 0) 1)
        ((= (modulo i 3) 1) 1)
        ((= (modulo i 3) 2) (* 2 (/ (+ i 1) 3)))))

; cont-frac
(define (cont-frac n d k)
  (define (iter i)
    (if (> k i)
        (/ (n i) (+ (d i) (iter (+ i 1))))
        (/ (n i) (d i))))
  (iter 1))

; e の近似値
(define (e-in)
  (define (iter k)
    (if (close-enough?
          (cont-frac (lambda (i) 1.0)
                     D
                     k)
          (- (exp 1) 2))
        (begin (display "k   = ")
               (display k)
               (newline)
               (display "c_f = ")
               (display (+ 2
                           (cont-frac (lambda (i) 1.0)
                                      D
                                      k)))
               (newline)
               (display "e   = ")
               (display (exp 1)))
        (iter (+ k 1))))
  (iter 1))


; run
(e-in)

; 結果
; k   = 5
; c_f = 2.71875
; e   = 2.718281828459045
;