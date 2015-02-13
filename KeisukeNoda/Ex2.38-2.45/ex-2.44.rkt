#lang planet neil/sicp

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
       ;再帰呼び出しは自分を呼び出すように修正
      (let ((smaller (up-split painter (- n 1))))
        ;まず上下に分けて、更に上側を横方向に分けて smaller を呼び出す。
        (below painter (beside smaller smaller)))))

;(paint (right-split einstein 3))
(paint (up-split einstein 3))