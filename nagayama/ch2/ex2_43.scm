#lang scheme

; Nクイーン問題
; lambda式の入れ子関係の違いによる計算時間の変化

; ex2.42 で用いたもの
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap 
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


; Louis が書いたもの
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap 
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
   (queen-cols board-size))

#|

 8クイーン問題を解く際に,
 queen-cols が何回呼ばれるかを考える.
 ex2.42 で用いた方は,

 (queen-cols 8) -> (queen-cols 7) -> ... -> (queen-cols 1)

 と順番に 8回 呼ばれる.
 Louis が書いた方は

 (queen-cols 8) -> (queen-cols 7)*8
 (queen-cols 7) -> (queen-cols 6)*8
 ...            -> ...
 (queen-cols 2) -> (queen-cols 1)*8
 
 と言ったように各段階で 8回呼ばれる.
 全体では (8^8 -1)/7 回呼ばれる.

 T = 8 より,
 Louis のプログラムが
 8クイーン問題を解くのにかかる時間は,
 
  (8^8 -1)/(7*8) * T  ~  8^6 * T
 
 となる.
 
|#



         
