#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;;で実行され、angleを3で割って0.1未満になるまで割った回数がpの手続きの回数なので、12.15では5回。
;;またこの関数は枝分かれすることがないのでスペースとステップは等しく、angleの値がaの値が3倍になると1増えるのでO(log(3,n))となる。