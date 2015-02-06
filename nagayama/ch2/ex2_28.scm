#lang racket

; fringe
; リストとして表現された木の葉を左から右への順で要素としたリストを返す


(define (fringe sl)
  (define (iter s t)
    (if (list? s)
        (if (null? s)
            t
            (iter (cdr s) (append t (iter (car s) (list)))))
        (list s)))
  (if (list? sl)
      (iter sl (list))
      #f))

(define x (list (list 1 2) (list 3 4)))
x                    ; ((1 2) (3 4))
(fringe x)           ; (1 2 3 4)
(fringe (list x x))  ; (1 2 3 4 1 2 3 4)


