#lang scheme

; (sum-triple n s)
; 和がs になる n以下の自然数i,j,k を求める手続き
; flatmap 

; 集積
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; flatmap
(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

; 範囲列挙
(define (enumerate-interval low high)
    (if (> low high)
        null
        (cons low (enumerate-interval (+ low 1) high))))

; i,j,k の和 をリストに加える
(define (add-sum-triple triple)
  (let ((i (car triple))
        (j (cadr triple))
        (k (caddr triple)))
       (list i j k (+ i j k))))

; i,j,k の全組み合わせ列挙
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; main
(define (sum-triples n s)
  (define (sum-triple-equal? triple s)
    (= (cadddr (add-sum-triple triple)) s))
  (map add-sum-triple
       (filter (lambda (triple) (sum-triple-equal? triple s))
               (unique-triples n))))

; run
(sum-triples 7 10)
