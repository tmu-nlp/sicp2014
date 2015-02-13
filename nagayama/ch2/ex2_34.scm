#lang scheme

; Horner's rules (ホーナー法)
; 集積を用いて手続きを定義する

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms))) ; <??>
              0
              coefficient-sequence))


; 集積
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; f(x) = 1 + 3x + 5x^3 + x^5
; f(2) = ?
(horner-eval 2 (list 1 3 0 5 0 1))


; f(2) = 1 + 3*2 + 5*8 + 32
;      = 1 +  6  +  40 + 32
;      = 79