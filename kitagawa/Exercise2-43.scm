(define (unique-trios n)
  (flatmap (lambda (li) (map (lambda (i) (append li (list i))) (enumerate-interval 1 (- (cadr li) 1))))
    (unique-pairs n)))

(define (trio-matching-sum n s)
  (filter (lambda (li) (= s (+ (car li) (cadr li) (caddr li))))
    (unique-trios n)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))
(define empty-board (list))
(define (adjoin-position row column list-of-list)
  (append (list (list row column)) list-of-list))

(define (safe? k list-of-list)
  (let ((kth (car list-of-list))) ;adjoin-positionにおいて最初のリスト(car list-of-list)のみ新しくappendされた要素なのでその部分のみ他の要素と衝突しないかどうか見れば良い。
       (define (iter rest)
         (cond ((null? rest) #t)
               ((conflicts? (car rest) kth) #f)
               (else (iter (cdr rest)))))
       (iter (cdr list-of-list))))

(define (conflicts? a b)
  (let ((dx (abs (- (car a) (car b))))
        (dy (abs (- (cadr a) (cadr b)))))
       (cond ((= dx 0) #t)
             ((= dy 0) #t)
             ((= dx dy) #t)
             (else #f))))


(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))


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


(print (queens 4))
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

(print (queens 4))

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
