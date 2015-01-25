(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        ()
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
    (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (r) (matrix-*-vector cols r)) m)))

(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define v (list 1 0 0))
(print (dot-product v v))
(print (matrix-*-vector m v))
(print (transpose m))
(print (matrix-*-matrix m m))
