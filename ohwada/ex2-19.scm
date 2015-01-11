(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))


(define (first-denomination x) (car x))
(define (except-first-denomination x) (cdr x))
(define (no-more? x) (null? x))



(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(print (cc 100 us-coins))
(print (cc 100 uk-coins))

(define us-coins (list 1 50 25 5 10))

(print (cc 100 us-coins))


; coin-values の順は cc の答えに影響はしない。
; それは可能性を全部調べるという点では変わりがないから。
