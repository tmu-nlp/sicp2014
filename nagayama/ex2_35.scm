#lang scheme

; count-leaves 集積として再定義する

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                    (if (pair? x)
                        (count-leaves x)
                        1))
                   t)))


; 集積
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; run
(define test-tree (list 1 (list 1 1)))
(count-leaves test-tree)
