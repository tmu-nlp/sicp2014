(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))


(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((< xu 0)         
           (cond ((< yu 0)  
                  (make-interval (* xu yu) (* xl yl)))
                 ((> yl 0)    
                  (make-interval (* xl yu) (* xu yl)))
                 (else
                  (make-interval (* xl yu) (* xl yl)))))
          ((> xl 0)
           (cond ((< yu 0)
                  (make-interval (* xu yl) (* xl yu)))
                 ((> yl 0)
                  (make-interval (* xl yl) (* xu yu)))
                 (else 
                  (make-interval (* xu yl) (* xu yu)))))
          (else
            (cond ((< yu 0)
                   (make-interval (* xu yl) (* xl yl)))
                  ((> yl 0)
                   (make-interval (* xl yu) (* xu yu)))
                  (else
                    (let ((p1 (* xl yl))
                          (p2 (* xl yu))
                          (p3 (* xu yl))
                          (p4 (* xu yu)))
                          (make-interval (if (< p2 p3) p2 p3)
                                         (if (> p1 p4) p1 p4)))))))))
   

 


(print (mul-interval (make-interval 4 6) (make-interval 3 4)))
(print (mul-interval (make-interval 4 6) (make-interval -3 -4)))
(print (mul-interval (make-interval 4 6) (make-interval 3 -4)))
(print (mul-interval (make-interval -4 -6) (make-interval 3 4)))
(print (mul-interval (make-interval -4 -6) (make-interval -3 -4)))
(print (mul-interval (make-interval -4 -6) (make-interval 3 -4)))
(print (mul-interval (make-interval 4 -6) (make-interval 3 4)))
(print (mul-interval (make-interval 4 -6) (make-interval -3 -4)))
(print (mul-interval (make-interval 4 -6) (make-interval 3 -4)))

