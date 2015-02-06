#lang racket

; equal? 手続きの定義
; 文字列リストの比較

(define (equal? a b)
  (cond ((and (null? a) (null? b))
         #t)
        ((not (or (null? a) (null? b)))
         (if (eq? (car a) (car b))
             (equal? (cdr a) (cdr b))
             #f))
        (else #f)))

; run
(equal? '(a b c d) '(a b c d))
(equal? '(a b c d) '(a '(b c) d))