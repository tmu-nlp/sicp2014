(define (min a b c)
  (if (< a b)
    (if (< a c) a c)
    (if (< b c) b c)))
        
(define (f a b c)
  (-
    (+
      (* a a)
      (* b b)
      (* c c))
    (* (min a b c) (min a b c))))

;(print (f 1 3 5))
