#lang scheme

; last-pair : 与えられたlistの最後の要素のみを持つリストを出力する 

(define (last-pair s)
  (if (list? s)
      (if (null? (cdr s))
          (list (car s))
          (last-pair (cdr s)))
      #f))

; run
(last-pair (list 23 72 149 34))
(last-pair (cons 23 (cons 72 (cons 149 34))))