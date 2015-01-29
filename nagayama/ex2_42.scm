#lang scheme

; Nクイーン問題 の正解を全探査する 手続きの設計


;; 集積
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; flatmap
(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

;; enumerate-interval : 範囲列挙
(define (enumerate-interval low high)
    (if (> low high)
        null
        (cons low (enumerate-interval (+ low 1) high))))


;; main
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        ; filter : k番目の要素がルール違反をしている候補を削る
        (filter
         (lambda (positions) (safe? k positions))
         ; flatmap : 再帰し, k-1 列目までで考えたときの正解を求める
         (flatmap 
          (lambda (rest-of-queens)
            ; map : 各正解候補の k 行目に (k,1) を追加したもの
            ;       各正解候補の k 行目に (k,2) を追加したもの
            ;       ...
            ;       各正解候補の k 行目に (k,n) を追加したもの
            ;       を生成する
            ;       (1辺8なら, 正解候補リストの要素数は8倍になる)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; empty-board : 空集合(チェス盤が0行ならば正解は0通り)
(define empty-board (list))

;; adjoin-position : 正解候補に駒を追加する手続き
(define (adjoin-position new-queen-row k rest-of-queens)
  (cons (list new-queen-row k) rest-of-queens))
  ;     \--------------------/ \-------------/
  ;         新しく置く駒の座標   既に置いてある駒の座標

;; safe? : k番目の駒(リストの先頭の要素) がルールを満たしているか確認する 
(define (safe? k positions)
  ; conflict? : 2つの駒について, 斜めか真横にあれば #t を返す手続き
  (define (conflict? A B)
    (let ((dy (abs (- (car A)  (car B) )))
          (dx (abs (- (cadr A) (cadr B)))))
      (or (= dx dy)
          (= dy 0))))
  ; iter : 既に置いてある各クイーンについて再帰ループ
  ;        k番目のクイーンがルールを満たしていれば #t を返す
  (define (iter A queens)
    (cond ((null? queens) #t)
          ((conflict? A (car queens)) #f)
          (else (iter A (cdr queens)))))
  ; iter 実行
  (iter (car positions) (cdr positions)))


;; run
(define answer (queens 5))
answer
(display "正解数 : ")
(length answer)

#|
- チェス盤面の座標
        \ x,column
  y,row  \ 1 2 3 4 
        1
        2
        3
        4

 座標   : (x, y)
 リスト : (row column)
 x,y表示 と row,column表示 では左右の数字が逆になる
 ややこしい


  

 N * N のチェス盤だとする(Nは十分大きい)

 1 列目 は N通り
  -> ( (1 1) (2 1) ... (N 1) )
  -> safe? は素通り

 N通りのそれぞれについて 2列目の全ての座標を追加
  -> ( ((1 2) (1 1)) ((2 2) (1 1)) ... ((N 2) (1 1))
       ((1 2) (2 1)) ((2 2) (2 1)) ... ((N 2) (2 1))
       ...                             ...
       ((1 2) (N 1)) ((2 2) (N 1)) ... ((N 2) (N 1)) )
  -> 要素数 N^2
  -> safe? を各要素に適用してカットすると
  -> 要素数は (N-1)(N-2)

 3列目も2列目と同様にして
  -> 要素数 N(N-1)(N-2)
  -> safe? を各要素に適用してカットすると
  -> 要素数は高々 (5(N-3)^2 + 2N)



|#