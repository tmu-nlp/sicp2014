;ex1_29.scm
(define (inc x) (+ x 1))
(define (cube x) (* x x x))

;a,b,term,next
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;f(x)
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;Simpson 
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (make-y k) (f (+ a (* k h)))) ;y_k
  (define (term k)
    (cond ((or (= k 0) (= k n)) (make-y k))
          ((even? k) (* (make-y k) 2))
          (else (* (make-y k) 4))))
  (* (/ h 3.0) (sum term 0 inc n)))
 

"(simpson-integral cube 0 1 100)"
(simpson-integral cube 0 1 100)

"(simpson-integral cube 0 1 1000)"
(simpson-integral cube 0 1 1000)
