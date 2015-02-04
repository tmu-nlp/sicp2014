; 2-46 で定義したベクトル表現

(define (make-vect x y) (cons x y))
(define (xocr-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))


(define (add-vect a b)
  (make-vect (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(define (sub-vect a b)
  (make-vect (- (car a) (car b)) (- (cdr a) (cdr b))))

(define (scale-vect a s)
  (make-vect (* (car a) s) (* (cdr a) s)))



; 平面上の有向線分の constructor と selector

(define (make-segment vect1 vect2) (cons vect1 vect2))

(define (start-segment segment) (car segment))
(define (end-segment segment)
  (add-vect (car segment) (cdr segment)))
