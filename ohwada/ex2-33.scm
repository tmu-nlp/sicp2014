(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))



(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(print (map sqrt (list 4 6 9))) ; (2 2.449..., 3)


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(print (append (list 1 3) (list 2 4 1))) ; (1 3 2 4 1) ← (cons 1 (cons 3 (list 2 4 1)))


(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(print (length (list 2 4 3 5 1 6))) ; 6
