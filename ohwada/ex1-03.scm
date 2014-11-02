(define (f a b c)
  (cond ((and (> a c) (> b c)) (+ (* a a) (* b b))) 
        ((and (> a b) (> c b)) (+ (* a a) (* c c)))
        ((and (> b a) (> c a)) (+ (* b b) (* c c)))))

(print (f 1 2 3))
