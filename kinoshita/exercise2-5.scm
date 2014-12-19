;ある数の素因数分解の結果は一意であるから，
;a,bのペアを(2^a)*(3^b)とすると素因数分解によりa,bを得ることができる

(define (fast-expt b n a)
    (cond
        ((= n 0) a)
        ((even? n) (fast-expt (square b) (/ n 2) a))
        (else (fast-expt b (- n 1) (* a b)))))
(define (even? n)
    (= (remainder n 2) 0))
(define (multiple3? n)
    (= (remainder n 3) 0))

(define (cons a b)
    (* (fast-expt 2 a 1) (fast-expt 3 b 1)))
(define (car z)
    (define (car-iter z a)
        (if (even? z)
            (car-iter (/ z 2) (+ a 1))
            a))
    (car-iter z 0))
(define (cdr z)
    (define (cdr-iter z b)
        (if (multiple3? z)
            (cdr-iter (/ z 3) (+ b 1))
            b))
    (cdr-iter z 0))

(define z (cons 4 3))
(print z)
(print (car z))
(print (cdr z))

(define z (cons 0 0))
(print z)
(print (car z))
(print (cdr z))

(define z (cons 1 0))
(print z)
(print (car z))
(print (cdr z))
