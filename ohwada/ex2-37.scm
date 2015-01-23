(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init segs)
  (if (null? (car segs))
      ()
      (cons (accumulate op init (map car segs))
            (accumulate-n op init (map cdr segs)))))


; dot-product
(define (dot-product v w)
  (accumulate + 0 (map * v w)))


; matrix-*-vector
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))


; transpose
(define (transpose mat)
  (accumulate-n cons () mat))


; matrix-*-matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define vecA (list 3 1 4))
(define vecB (list 2 2 5))
(define matA (list (list 1 2 3) (list 2 3 4)))
(define matB (list (list 3 5) (list 2 4) (list 4 1)))


(print (dot-product vecA vecB)) ; 3*2 + 1*2 + 4*5 = 28
(print (matrix-*-vector matA vecA)) ; (1*3 + 2*1 + 3*4, 2*3 + 3*1 + 4*4) = (17, 25)
(print (transpose matA)) ; ((1 2) (2 3) (3 4))
(print (matrix-*-matrix matA matB)) ; ((1*3+2*2+3*4, 1*5+2*4+3*1), (2*3+3*2+4*4, 2*5+3*4+4*1)) = ((19, 16), (28, 26))
