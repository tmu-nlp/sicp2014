(module common racket
  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
  
  (define (accumulate-n op init seqs)
    (if (null? (car seqs))
        null
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))
  
  (define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))))
  
  (define (fold-left op initial sequence)
    (define (iter result rest)
      (if (null? rest)
          result
          (iter (op result (car rest))
                (cdr rest))))
    (iter initial sequence))
  
  (define (enumerate-interval low high)
    (if (> low high)
        null
        (cons low (enumerate-interval (+ low 1) high))))
  
  (define (flatmap proc seq)
    (accumulate append null (map proc seq)))
  
  ;; prime?
  (define (smallest-divisor n)
    (find-divisor n 2))
  
  (define (find-divisor n test-divisor)
    (cond ((> (sqr test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  
  (define (divides? a b)
    (= (remainder b a) 0))
  
  (define (prime? n)
    (= n (smallest-divisor n)))
  ;; end of prime?
  
  (provide accumulate 
           accumulate-n 
           fold-right 
           fold-left 
           enumerate-interval
           flatmap
           prime?))

