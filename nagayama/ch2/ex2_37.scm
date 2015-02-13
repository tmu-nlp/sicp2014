#lang scheme
; 行列を扱う手続きの設計
; 手続き中の <??> を補完する

; 前提関数
 ; n-集積
 (define (accumulate-n op init seqs)
   (if (null? (car seqs))
       (list)
       (cons (accumulate op init (map car seqs))
             (accumulate-n op init (map cdr seqs)))))
 ; 集積
 (define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

; 前提関数終わり


; ベクトル同士の内積
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; 行列*ベクトル
(define (matrix-*-vector m v)
   (map (lambda (x) (dot-product x v)) m))

; 転置
(define (transpose mat)
  (accumulate-n cons (list) mat))

; 行列*行列
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))


; run
(define vec1 (list  1  2  3))
(define vec2 (list -1 -1 -1))
(define mat1 (list (list  1  2  3)
                   (list  4  5  6)
                   (list  7  8  9)))
(define mat2 (list (list -1  0  0)
                   (list  0 -1  0)
                   (list  0  0 -1)))

(dot-product vec1 vec2)
(matrix-*-vector mat2 vec2)
(transpose mat1)
(matrix-*-matrix mat1 mat2)


