(define (enumerate-interval low high)
    (if (> low high)
        ()
        (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append () (map proc seq)))

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

(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))

(define (safe? k positions)
    (define (iter newQ oldQs count)
        (cond ((null? oldQs) #t)
              ((or (= newQ (car oldQs))
                   (= newQ (+ (car oldQs) count))
                   (= newQ (- (car oldQs) count)))
               #f)
              (else
                  (iter newQ (cdr oldQs) (+ 1 count)))))
    (if (null? positions)
        #t
        (iter (car positions) (cdr positions) 1)))

(print (queens 8))
