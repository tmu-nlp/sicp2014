#lang planet neil/sicp

;;(define (right-split painter n)
;;  (if (= n 0)
;;      painter
;;      (let ((smaller (right-split painter (- n 1))))
;;        (beside painter (below smaller smaller)))))
;;
;;(define (up-split painter n)
;;  (if (= n 0)
;;      painter
;;      (let ((smaller (up-split painter (- n 1))))
;;        (below painter (beside smaller smaller)))))


;; right-split、up-split に相当する手続き(spliter)を返す手続き(split)を定義する。
;; 共通するところを split に定義

(define (split t1 t2)
  (lambda (painter n)
    (if (= n 0)
        painter
      (let ((smaller ((split t1 t2) painter (- n 1))))  ;;手続きに対して painter と n-1 を渡す
        (t1 painter (t2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 3))
;(paint (up-split einstein 3))
