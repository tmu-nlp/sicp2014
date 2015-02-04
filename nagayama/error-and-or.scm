#lang scheme

; 環境 : DrRacket
;
; and,or を引数に取れないことの検証
;

; test 1
; and,or だと 構文error
(define (f x) (x 1))
(f not)               ; -> #f
(f and)               ; error
(f or)                ; error


; test 2
(define (g x)
  ((if (> x 0) cons and) #t #f))
(g 1)                 ; error


; test 3
; iter を挟んでも error
(define and-inte and)
(define (h x) (x #t #f))
(h and-inte)          ; error

; test 4
; これだと error が出ない
(define (and-inte-2 x y) (and x y))
(define (h2 x) (x #t #f))
(h2 and-inte-2)       ; -> #f
