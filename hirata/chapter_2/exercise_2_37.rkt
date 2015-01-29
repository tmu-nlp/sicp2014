(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs) 
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;dot
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;matrix*vector
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

;transpose(転置)
(define (transpose mat) 
  (accumulate-n cons '() mat)) 

;matrix*matrix
(define (matrix-*-matrix m n) 
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(display (dot-product (list 1 2 3) (list 4 5 6)))
(newline)
(display (matrix-*-vector '((2 -1) (-3 4)) '(1 2)))
(newline)
(display (transpose '((1 2 3) (4 5 6) (7 8 9))))
(newline)
(display (matrix-*-matrix '((1 -1) (-2 3)) '((1 2) (3 4))))