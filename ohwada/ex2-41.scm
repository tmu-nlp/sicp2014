; accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; flatmap
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

; enumerate-interval
(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))


; make-traid-sum
(define (make-traid-sum traid)
  (+ (car traid) (cadr traid) (caddr traid)))

; unique-traid
(define (unique-traid n)
  (flatmap (lambda (i)
           (flatmap (lambda (j)
                    (map (lambda (k) (list k j i))
                         (enumerate-interval 1 (- j 1))))
                    (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))



; traid-sum
(define (traid-sum n s)
  (filter (lambda (x)
            (if (= s (make-traid-sum x)) #t #f))
          (unique-traid n)))


(print (traid-sum 7 12)) ; ((3 4 5) (2 4 6) (1 5 6) (2 3 7) (1 4 7))
