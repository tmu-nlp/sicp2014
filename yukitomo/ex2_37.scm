;ex2_37.scm

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define v (list 1 2))
(define w (list 3 4))
(define m (list (list 1 2) (list 3 4)))

;内積
(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(dot-product v w)

;matrix-*-vector
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(matrix-*-vector m v)

;転置
(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose m)

;;matrix-*-matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
       (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix m m)

