#lang scheme

; チャーチ数
; チャーチ数は 1変数関数f と 引数x を引数にとる
; 自然数n は「基準値(引数x) に 関数f を n回 適用すること」と表現される


(define zero (lambda (f) (lambda (x) x)))
(define add-1
  (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))))

(define   one (lambda (f) (lambda (x) (f x))))
(define   two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

; 以下実行テスト

(define (inc x) (+ x 1))

; 和算
(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

; 出力
((zero inc) 0)  ; -> 0
((one inc) 0)   ; -> 1
((two inc) 0)   ; -> 2

(((add one two) inc) 0)  ; 1+2 = 3


#| 

[1] (add-1 zero) を展開して one になるか確認

    (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x) )))
    (lambda (f) (lambda (x) (f ((            (lambda (x) x))    x) )))
    (lambda (f) (lambda (x) (f                           x         )))
    -> one

|#

#|

[2] (((add one two) inc) 0) を展開して three が出てくるか確認


    ((( (lambda (f) (lambda (x) ((n f) ((m f) x)))) (lambda (f) (lambda (x) (f x))) (lambda (f) (lambda (x) (f (f x)))) ) inc) 0)
        \-------------------------*------*--------/ \-----------------------------/ \---------------------------------/
                          (add m n)                                one                              two

    (add m n) の m,n に one, two を代入

    ((  (lambda (f) (lambda (x) (( (lambda (f) (lambda (x) (f x))) f) (( (lambda (f) (lambda (x) (f (f x)))) f) x))))     inc) 0)
        \------------------------- \-----------------------------/ ----- \---------------------------------/ -------/  
                     add                        one                                  two

    ((  (lambda (f) (lambda (x) (( (lambda (f) (lambda (x) (f x))) f) (( (lambda (f) (lambda (x) (f (f x)))) f) x))))     inc) 0)
                                 \----------------------------------/  \--------------------------------------/  
                                    lambda (f) に f を代入                 lambda (f) に f を代入

    ((  (lambda (f) (lambda (x) (              (lambda (x) (f x))     (              (lambda (x) (f (f x)))     x))))     inc) 0)
                                                                      \------------------------------------------/  
                                                                                      lambda (x) に x を代入

    ((  (lambda (f) (lambda (x) (              (lambda (x) (f x))                                (f (f x))        )))     inc) 0)
                                \---------------------------------------------------------------------------------/
                                                                   スペースを詰める

    ((  (lambda (f) (lambda (x) ( (lambda (x) (f x)) (f (f x)) ))) inc) 0)
                                \------------------------------/  
                                   lambda (x) に (f (f x)) を代入する

    ((  (lambda (f) (lambda (x) (f (f (f x))) )) inc) 0)
        \--------------------------------------/  
                = three 

|#
