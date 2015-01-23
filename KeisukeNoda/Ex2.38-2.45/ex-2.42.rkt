#lang racket

(require "public.rkt")

(define empty-board '())

(define (adjoin-position row col rest-of-queens)
  (append rest-of-queens (list (list row col))))

(define (safe? k positions)
  (define (two-queens-safe? q1 q2)
    (and (not (= (car q1) (car q2)))
         (not (= (- (car q1) (cadr q1))
                 (- (car q2) (cadr q2))))
         (not (= (+ (car q1) (cadr q1))
                 (+ (car q2) (cadr q2))))))

  (let ((new-queen (last positions)))
    (define (check i positions)
      (cond ((= i k) true)
            ((two-queens-safe? (car positions) new-queen)
             (check (+ i 1) (cdr positions)))
            (else false)))
    (check 1 positions)))

(define (queens board-size)
  (define (queen-cols k)
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

(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
          (adjoin-position new-row 1 rest-of-queens))
        (enumerate-interval 1 5)))
 (list '()))

'(((1 1) (2 1) (3 1) (4 1) (5 1) 1 1) 
  ((1 1) (2 1) (3 1) (4 1) (5 1) 2 1) 
  ((1 1) (2 1) (3 1) (4 1) (5 1) 3 1) 
  ((1 1) (2 1) (3 1) (4 1) (5 1) 4 1) 
  ((1 1) (2 1) (3 1) (4 1) (5 1) 5 1))

(queens 8)