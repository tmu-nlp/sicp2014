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

#|
-- 証明問題 --

Fib(n) = \frac{\phi^n - \psi^n}{\sqrt 5} を
帰納法とフィボナッチの定義から証明。まず、
(1) Fib(0) = 0, Fib(1) = 1 なので n = 0, 1 のとき正しい
(2) n = k, k + 1 のとき
Fib(k)   = \frac{\phi^k     - \psi^k}{\sqrt 5}
Fib(k+1) = \frac{\phi^{k+1} - \psi^{k+1}}{\sqrt 5}
が、成り立つとすると、
n = k + 2 のとき
Fib(k+2) = Fib(k+1) + Fib(k)
         = \frac{\phi^{k+1} - \psi^{k+1}}{\sqrt 5} + \frac{\phi^k - \psi^k}{\sqrt 5}
         = \frac{\phi^k (k+1) - \psi^k (k+1)}{\sqrt 5}
         = \frac{\phi^k \phi^2 - \psi^k \psi^2}{\sqrt 5}
         = \frac{\phi^{k+2} - \psi^{k+2}}{\sqrt 5}
よって(1),(2)より、Fib(n) = \frac{\phi^n - \psi^n}{\sqrt 5} であることが証明された。
そして、
Fib_d(n) = \left| \frac{\phi^n - \psi^n}{\sqrt 5} - \frac{\phi^n}{\sqrt 5} \right| < 0.5 
であることが証明できればよいので、
　\left| \frac{\phi^n - \psi^n}{\sqrt 5} - \frac{\phi^n}{\sqrt 5} \right|
= \left| - \frac{\psi^n}{\sqrt 5} \right|
= \left| \frac{\psi^n}{\sqrt 5} \right|
ここで n = 1 のとき、
Fib_d(1) = \left| \frac{\psi^1}{\sqrt 5} \right| = 0.27639
であり、さらに
\left| \psi \right| < 1
なので、Fib_d(n) は減少関数
Fib(n) は \frac{\phi^n}{\sqrt 5} に最も近い整数である
