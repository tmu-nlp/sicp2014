;ex2_11.scm

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (new-mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((< xu 0)
           (cond ((< yu 0) (make-interval (* xu yu) (* xl yl)))
                 ((< yl 0) (make-interval (* xl yu) (* xl yl)))
                 (else (make-interval (* xl yu) (* xu yl)))))
          ((< xl 0)
           (cond ((< yu 0) (make-interval (* xu yl) (* xl yl)))
                 ((< yl 0) (make-interval (min (* xl yu) (* xu yl))
                                        (max (* xl yl) (* xu yu))))
                 (else (make-interval (* xl yu) (* xu yu)))))
          (else
           (cond ((< yu 0) (make-interval (* xu yl) (* xl yu)))
                 ((< yl 0) (make-interval (* xu yl) (* xu yu)))
                 (else (make-interval (* xl yl) (* xu yu))))))))

(define p (make-interval -2 8))
(define q (make-interval 3 9))
(mul-interval p q)
(new-mul-interval p q)
