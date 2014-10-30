;;;; Exercise 1.11 ;;;;

;; 再帰
(define (f1 n)
  (if (< n 3)
      n
      (+ ( f1 (- n 1))
         (* 2 (f1 (- n 2)))
         (* 3 (f1 (- n 3))))))

;; 反復
(define (f2 n) 
  (define (iter a b c count) 
    (if (= count 0) 
      a 
      (iter b c (+ c (* 2 b) (* 3 a)) (- count 1)))) 
  (iter 0 1 2 n)) 

;;;; Exercise 1.12 ;;;;

(define (pascal-triangle row col) 
  (cond ((> col row) 0) 
        ((< col 0) 0) 
        ((= col 1) 1) 
        ((+ (pascal-triangle (- row 1) (- col 1)) 
            (pascal-triangle (- row 1) col)))))

;;;; Exercise 1.13 ;;;;
