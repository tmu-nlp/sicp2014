;ex2_14.scm
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
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

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)) 
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2)) 

(define (make-center-percent c p)
  (make-interval (- c (* (/ c 100.0) p)) 
                 (+ c (* (/ c 100.0) p))))
(define (percent i)
  (* (/ (width i) (center i)) 100.0))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;test
(define A (make-interval 2 10))
(define B (make-interval 3 6))

(par1 A B)
(par2 A B)

(define A (make-center-percent 10 1))
(define B (make-center-percent 15 1))

(par1 A B)
(par2 A B)

