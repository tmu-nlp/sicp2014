;ex1_30.scm
(define (inc x) (+ x 1))
(define (cube x) (* x x x))

;a,b,term,next

;recursion
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

;iteretion
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;Simpson 
(define (simpson-integral f a  b n)
  (define h (/ (- b a) n))
  (define (make-y k) (f (+ a (* k h)))) ;y_k
  (define (term k)
    (cond ((or (= k 0) (= k n)) (make-y k))
          ((even? k) (* (make-y k) 2))
          (else (* (make-y k) 4))))
  (* (/ h 3) (sum term 0 inc n)))
 

"(simpson-integral cube 0 1 100)"
(simpson-integral cube 0 1 100)

"(simpson-integral cube 0 1 1000)"
(simpson-integral cube 0 1 1000)
