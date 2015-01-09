#lang racket

;; 引数として数のリストをとり、2乗のリストを返す
;; square-listについて

"first"
(define (square-list1 items)
  (if (null? items)
      null
      (cons (sqr (car items)) (square-list1 (cdr items)))))

(square-list1 (list 1 2 3 4))
'(1 4 9 16)


"second"
(define (square-list2 items)
  (map (lambda (x) (sqr x)) items))

(square-list2 (list 1 2 3 4))
'(1 4 9 16)