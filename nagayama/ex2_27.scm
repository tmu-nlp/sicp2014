#lang scheme

; deep-reverse : ex2.18 の reverse 手続きの改変
; サブリストの中身も逆順になるように書き換える

(define (deep-reverse s)
  (define (iter s t)
    (if (list? s)   
        (if (null? s)
            t
            (iter (cdr s) (cons (iter (car s) (list)) t)))
        s))
  (if (list? s)
      (iter s (list))
      #f))

(define (reverse s)
  (define (iter s t)
    (if (null? s)
        t
        (iter (cdr s) (cons (car s) t))))
  (if (list? s)
      (iter s (list))
      #f))

; run
(define x (list (list 1 2) (list 3 4)))
x                 ; ((1 2) (3 4))
(reverse x)       ; ((3 4) (1 2))
(deep-reverse x)  ; ((4 3) (2 1))

