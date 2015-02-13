#lang racket

; 整数, 有理数, 実数, 複素数 による型の塔について
; 型のレベルを 1つ上げる手続き raise を定義する.



(define (raise x) (apply-generic 'raise x))

; -> install-scheme-number-package
  (put 'raise '(scheme-number)
       (lambda (x)
         (if (exact-integer? x)
             (make-rational x 1)
             (make-complex-from-real-imag x 0))))

; -> install-rational-package
  (put 'raise '(rational)
       (lambda (r)
         (make-scheme-number (exact->inexact (/ (numer r) (denom r))))))


; 実行テストは ex2_84

