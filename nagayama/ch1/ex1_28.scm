; #lang scheme

; miller-rabin 法 による素数判定.


; 前提
(require srfi/27) ; Sources of Random Bits
(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))



; n = (2^k)*q を満たす k,q を求める
(define (k&q n)
    (let loop ((k 0)
               (q n))
      (if (even? q)
          (loop (+ k 1) (/ q 2))
          (values k q))))

; べき乗の剰余
(define (modexpt m base n)
  (let loop ((base base)
             (n n)
             (acc 1))
    (cond ((= n 0) acc)
          ((even? n)
           (loop (remainder (square base) m)
                 (/ n 2)
                 acc))
          (else
           (loop base
                 (- n 1)
                 (remainder (* base acc) m))))))

; miller-rabin 法
(define (miller-rabin n base)
    (call-with-values
      (lambda () (k&q (- n 1)))
      (lambda (k q)
        (let loop ((i 0)
                   (r (modexpt n base q)))
          (and (< i k)
               (cond ((= r 1) (= i 0))
                     ((= r (- n 1)) #t)
                     (else
                       (loop (+ i 1)
                             (remainder (square r) n)))))))))

; main
(define (miller-test n)
  (define (iter n count)
    (if (< count 1)
        #t
        (if (miller-rabin n (random-integer n))
            (iter n (- count 1))
            #f)))
  (cond ((not (> n 1)) #f)
        ((or (= n 2)
             (= n 3)
             (= n 5)
             (= n 7)) #t)
        (else (iter n test-times))))

; 試行回数の設定
; 10回だと six nines
(define test-times 10)


; run
(miller-test 11027) ; -> #t





