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
