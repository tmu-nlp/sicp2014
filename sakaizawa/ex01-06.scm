#!usr/bin/gosh

;; -*- coding:utf-8 -*-

;;この問題は無限ループするのでその説明だけ書きます
;;ifの場合は特殊形式でgood-enough?の結果によってどちらかを選択する
;;condの場合すべての条件式をどちらも実行した後に選択する
;;これによりsqrt-iterを呼び出し続けて無限ループに入る

(define (sqrt-iter guess x)
 (if (good-enough? guess x)
  guess
   (sqrt-iter (improve guess x)
    x)))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

