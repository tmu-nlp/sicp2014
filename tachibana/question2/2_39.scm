(define (reverse items)
  (define (iter rev rest)
    (if (null? rest)
        rev
        (iter (cons (car rest) rev) (cdr rest))))
  (iter (list) items))

(define (fringe tree)
  (define (iter return rest)
    (if (null? rest)
        return
        (if (list? (car rest))
          (iter (append return (iter (list) (car rest))) (cdr rest))
        (iter (append return (list (car rest))) (cdr rest))))
  )
  (iter (list) tree)
)

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(print (reverse (list 1 2 3 4 5 6)))

;もっとスマートな方法あると思うけど意地になってone-linerみたいになってるけど解いた。
(define (reverse sequence)
  (fold-right (lambda (x y) (cond ((null? y) x) ((list? y) (fringe (list y x))) (else (list y x)))) (list) sequence))

(print (reverse (list 1 2 3 4 5 6)))


(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))

(print (reverse (list 1 2 3 4 5 6)))


(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))

(print (reverse (list 1 2 3 4 5 6)))
