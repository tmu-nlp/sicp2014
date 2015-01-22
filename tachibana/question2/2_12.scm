(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* (/ c 100.0) p))
                 (+ c (* (/ c 100.0) p))))

;区間のパーセント相対許容誤差とは中央値に対して、幅がどのくらいの割合か示す割合であるため、
(define (percent i)
  (* (/ (width i) (center i)) 100.0))


(print "(make-center-percent 50 15)")
(print (make-center-percent 50 15))

(print "(percent (cons 42.5 57.5))")
(print (percent (cons 42.5 57.5)))
