(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (cond 
        ((>= (lower-bound x) 0)
            (cond
                ((>= (lower-bound y) 0)
                    (make-interval
                        (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
                ((< (upper-bound y) 0)
                    (make-interval
                        (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (upper-bound y))))
                (else
                    (make-interval
                        (* (upper-bound x) (lower-bound y))
                        (* (upepr-bound x) (upper-bound y))))))
        ((< (upper-bound x) 0)
            (cond
                ((>= (lower-bound y) 0)
                    (make-interval
                        (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
                ((< (upper-bound y) 0)
                    (make-interval
                        (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
                (else
                    (make-interval
                        (* (lower-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))))
        (else
            (cond
                ((>= (lower-bound y) 0)
                    (make-interval
                        (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (upper-bound y))))
                ((< (upper-bound y) 0)
                    (make-interval
                        (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (lower-bound y))))
                (else
                    (let ((p1 (* (lower-bound x) (lower-bound y)))
                          (p2 (* (lower-bound x) (upper-bound y)))
                          (p3 (* (upper-bound x) (lower-bound y)))
                          (p4 (* (upper-bound x) (upper-bound y))))
                    (make-interval
                        (min p2 p3)
                        (max p1 p4))))))))

(define (div-interval x y)
    (if (= (- (upper-bound y) (lower-bound y)) 0)
        -1
        (mul-interval
            x
            (make-interval (/ 1.0 (upper-bound y))
                           (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
    (add-interval
        x
        (make-interval (* -1.0 (upper-bound y))
                        (* -1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound intr)
    (cdr intr))

(define (lower-bound intr)
    (car intr))

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
    (make-center-width c (* c (/ p 100))))

(define (percent i)
    (* (/ (width i) (center i)) 100))

(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval
            one (add-interval (div-interval one r1)
                              (div-interval one r2)))))

(define r1 (make-center-percent 6.8 5))
(define r2 (make-center-percent 4.7 1))
(define r3 (make-center-percent 5 1))

(print (par1 r1 r2))
(print (par2 r1 r2))
(newline)
(print (par1 r1 r3))
(print (par2 r1 r3))
(newline)
(print (par1 r2 r3))
(print (par2 r2 r3))
