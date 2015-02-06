#lang scheme

; reverse を fold-right, fold-left を用いて定義

; reverseR (fold-right)
(define (reverseR sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))

; reverseL (fold-left)
(define (reverseL sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))


; fold-right
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define fold-right accumulate)

; fold-left
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


; run
(reverseR (list 1 2 3))
(reverseL (list 1 2 3))

#|

(1 2 3)

; fold-right

  (f 1 (f 2 (f 3 x)))

  (1 (2 ())) -> (2 ()), (1 ())
             -> (2 (1 ())

 (lambda (x y) (append y (list x)))


; fold-left

  (f (f (f x 1) 2) 3)

  (lambda (x y) (cons y x))

|#

