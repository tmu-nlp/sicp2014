;;; 2.42 の実装
(define (queens board-size)
  (define (queen-cols k)
    (print "queen-cols is called.")
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


;;; 2.43 の実装
(define (queens board-size)
  (define (queen-cols k)
    (print "queen-cols is called.")
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))


;;; 実行を遅くする理由
;;; (queen-cols (- k 1)) の計算が重複して実行されるため。

;;; queen-cols が呼ばれる回数を時間として考える
;;;
;;; board-size = 1 のとき
;;; 2.42 の実装: 2回
;;; 2.43 の実装: 2回
;;;
;;; board-size = 2 のとき
;;; 2.42 の実装: 3回
;;; 2.43 の実装: 7回
;;;
;;; board-size = 3 のとき
;;; 2.42 の実装: 4回
;;; 2.43 の実装: 40回
;;;
;;; board-size = n のとき
;;; 2.42 の実装: n + 1回
;;; 2.43 の実装: 1 + n^1 + n^2 + n^3 + ... + n^n
;;;
;;; 2.42 で問題を解く時間をTとしたとき、
;;; 2.43 で問題を解くのにかかる時間は、およそT^n となる
