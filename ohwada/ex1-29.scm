; sum を定義
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; integral を定義
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; simpson を定義
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (coefficient k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (y k)
    (* (f (+ a (* k h))) (coefficient k)))
  (* (sum y 0 inc n)
     (/ h 3.0)))

(define (inc x) (+ x 1))
(define (even? x) (= (remainder x 2) 0))
(define (cube x) (* x x x))



(print (integral cube 0 1 0.01))
(print (integral cube 0 1 0.001))

(print (simpson cube 0 1 100))
(print (simpson cube 0 1 1000))

