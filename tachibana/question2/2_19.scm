(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values))
        )
  )
)

(define (no-more? coin_list) (null? coin_list))
(define (except-first-denomination coin_list) (cdr coin_list))
(define (first-denomination coin_list) (car coin_list))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define ja-coins (list 1 5 10 50 100 500))

(print "(cc 100 us-coins)")
(print (cc 100 us-coins))
