(define (queens board-size)
  (define (queen-cols k)
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


; accumulate, flatmap, enumerate-interval
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))


; adjoin-position, empty-board
(define (adjoin-position new-low k rest-of-queens)
  (append rest-of-queens (list (list new-low k))))

(define empty-board ())

; safe?
(define (safe? k positions)
  (define (judge-iter x y)
    (cond ((null? y) #t)
          ((= (car x) (caar y)) #f)
          ((= (- (car x) (caar y))
              (- (cadr x) (cadar y))) #f)
          ((= (- (car x) (caar y))
              (- (cadar y) (cadr x))) #f)
          (else (judge-iter (car (reverse positions)) (cdr y)))))
  (judge-iter (car (reverse positions)) (cdr (reverse positions))))





(print (queens 7))

; 遅くなる理由:
;  2.42 のプログラムは queen-cols が bordsize 分の回数呼ばれるだけだが、
;  2.43 のプログラムは enumerate-interval が生成する 1 ~ bordsize に対応する
;  各行毎に queen-cols が bordsize 分の回数呼ばれるから。

; 2.43 のプログラムがエイトクイーンパズルを解くのにかかる時間:
; 8^8 / T(=8) = 8^7 * T ほど(?) 
