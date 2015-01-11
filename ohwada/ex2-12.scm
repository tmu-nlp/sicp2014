; make-interval

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))


; make-center-width, center, width

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


; make-center-persent, persent

(define (make-center-persent c p)
  (make-interval (- c (* c (/ p 100.0))) (+ c (* c (/ p 100.0)))))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))


(print (center (make-center-persent 40 20)))
(print (percent (make-center-persent 40 20)))
(print (make-center-persent 40 20)) ; 40 * 0.2 = 8 なので (32, 48)
