#lang racket

;; count-leavesを流用する
(define (fringe items)
  (cond ((null? items) null)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items)) (fringe (cdr items))))))


(define x (list (list 1 2) (list 3 4)))
x

(fringe x)
'(1 2 3 4)

(fringe (list x x))
'(1 2 3 4 1 2 3 4)
