; 再帰的
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(print (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)) ; 4 桁の精度


; 反復的
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(print (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 38)) ; これ以降答えが変わらなくなる
