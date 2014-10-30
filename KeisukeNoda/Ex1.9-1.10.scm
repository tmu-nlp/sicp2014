;;;; Exercise 1.9 ;;;;

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (r-plus a b)
  (if (= a 0) 
      b
      (inc (r-plus (dec a) b))))

(define (i-plus a b)
  (if (= a 0)
      b
      (i-plus (dec a) (inc b))))

(+ 4 5)
(r-plus 4 5)
(i-plus 4 5)

;;; result ;;;

;; gosh> 9

;; gosh> 9
;;
;; 再帰である。
;; 以下、プロセス（式展開）
;;   (r-plus 4 5) 
;;   (inc (r-plus (dec 4) 5)) 
;;   (inc (inc (r-plus (dec 3) 5))) 
;;   (inc (inc (inc (r-plus (dec 2) 5)))) 
;;   (inc (inc (inc (inc (r-plus (dec 1) 5))))) 
;;   (inc (inc (inc (inc (r-plus 0 5)))) 
;;   (inc (inc (inc (inc 5))) 
;;   (inc (inc (inc 6))) 
;;   (inc (inc 7)) 
;;   (inc 8) 
;;   9

;; gosh> 9
;;
;; 反復である。
;; 以下、プロセス（式展開）
;;   (plus2 4 5) 
;;   (plus2 (dec 4)(inc 5)) 
;;   (plus2 (dec 3)(inc 6)) 
;;   (plus2 (dec 2)(inc 7)) 
;;   (plus2 (dec 1)(inc 8)) 
;;   9


;;;; Exercise 1.10 ;;;;

(define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1)
                    (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(f 4)
(g 4)
(h 4)
(k 4)

;;; result ;;;

;; gosh> 1024
;; gosh> 65536
;; gosh> 65536
;;
;; gosh> 8
;; gosh> 16
;; gosh> 65536
;; gosh> 80

;; (f n) ;;
;; f(n) = 2n であるので、f(4) = 2*4 = 8

;; (g n) ;;
;; (A 1 n)
;; (A 0 (A 1 (- n 1)))
;; (* 2 (A 1 (- n 1))) となり、(- n 1) が 1 になるまで　
;; (* 2 (* 2 (* ...) と増え続けてくので
;; g(n) = 2^n であるので、 g(4) = 2^4 = 16

;; (h n) ;;
;; (A 2 n)
;; (A 1 (A 2 (- n 1))) となり、 (- n 1)が 1 になるまで
;; (A 1 (A 1 (A 1 ....(A 2 1) と増え続けていく. (A 2 1) = 1より、
;;                       ^ n個目
;; (A 1 (A 1 (A 1 ...(A 1 2)　であり、
;;                      ^ n-1個目
;; (A 1 (A 1 (A 1 ...(g 2)
;;                     ^ n-1個目
;; (g (g (g ...(g 1) であるので、
;;               ^  n個目
;; (h n) = (g 2^n) (n ≠ 0, 1)よって、
;; h(0) = 0, h(1) = 2, h(n) = ( h(n-1) )^2 であるので
;; h(4) = 2^2^2^2 = 2^16 = 65536
;;
;; (k n) ;;
;; k(n) = 5n^2 = 5*16 = 80
