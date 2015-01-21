(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))


(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))


(define (unique-pairs n)
(flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n)))

(define (unique-trios n)
  (flatmap (lambda (li) (map (lambda (i) (append li (list i))) (enumerate-interval 1 (- (cadr li) 1))))
    (unique-pairs n)))


(define (trio-matching-sum n s)
  (filter (lambda (li) (= s (+ (car li) (cadr li) (caddr li))))
    (unique-trios n)))






































