(define (unique-pairs n)
(flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n)))

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

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

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

(print (queens 4))

