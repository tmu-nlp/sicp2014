(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define v (list 1 2))
(print "v: " v)
(define w (list 3 4))
(print "w: " w)
(define m (list (list 1 2) (list 3 4)))
(print "m: " m)

;;内積
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(print "(dot-product v w)")
(print (dot-product v w))

;;行列とベクトルの内積
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

;;転置
(define (transpose mat)
  (accumulate-n cons (list) mat))

;;行列と行列の内積
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (w) (matrix-*-vector cols w)) m)))

