(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

; (accumulate append (list)
;             (map (lambda (i)
;                    (map (lambda (j) (list i j))
;                         (enumerate-interval 1 (- i 1))))
;                  (enumerate-interval 1 n)))

;(map +  (list)) のようにmapに引数(list)を渡すと()が返ってくる

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;prime?が定義されてない？

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (permutations s)
  (if (null? s)
      (list (list))
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))




(define (unique-pairs n)
(flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n)))


;;prime?が定義されてないため確認できない
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))









































