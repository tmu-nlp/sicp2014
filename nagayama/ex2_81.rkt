#lang racket


; apply-generic
; 同型の強制型変換は必要かどうか


#|

[a]
apply-generic について考える.
命令に対する2つの引数の型が scheme-number,
または 2つの引数の型が complex である.
それらの型に対応する命令がテーブルに無かったときどうなるか.

型変換表に同じ型への変換が存在するので,
同型変換が繰り返され無限ループが発生する.

; |#


#|
[b]
同じ型の引数の強制型変換について何かすべきだという
Louis の意見は正しいかどうか.

[a] の結果より, 正しくない.

; |#

#|
[c]
二つの引数が同じ型を持っていれば
強制型変換を試みないように apply-generic を書き換える.
; |#

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

