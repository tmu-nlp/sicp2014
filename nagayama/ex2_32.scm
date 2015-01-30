#lang scheme

; 部分集合の集合のリストを作成する手続き
; 手続きの定義の <??> を補完する
;
; 両替えプログラム, 分配法則, などのイメージ

(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map  
                      (lambda (t) (cons (car s) t)) ; <??>
                      rest)))))

(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items)))))
          
; run
(subsets (list 1 2 3))  ; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(subsets (list   2 3))  ; (() (3) (2) (2 3))
(subsets (list     3))  ; (() (3))

;                        |       |         |                       |
;                         3 だけ  2を加えると       1も使うと

#|

(let ((rest (subsets (cdr s))))
 rest : 先頭の要素を除いた集合の部分集合の集合
 
(append rest (map <??> rest))
             \-------------/
   先頭の要素を含む部分集合の集合 になって欲しい
 
<??> -> (lambda (t) (cons (car s) t))
 rest の各要素に 先頭の要素を加えれば良い

|#