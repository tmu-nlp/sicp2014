#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-20.scm

(define (gcd a b)
  (if (= b 0)
    a   
    (gcd b (remainder a b))))

; norma-order eval
; 18
(gcd 206 40) 

(if (= 40 0) ; #f
      206 
      (gcd 40 (rem 206 40)))

(if (= (rem 206 40) 0) ; #f
      40  
      (gcd (rem 206 40) (rem 40 (rem 206 40))))

(if (= (rem 40 (rem 206 40)) 0) ; #f
      (rem 206 40) 
      (gcd (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))

(if (= (rem (rem 206 40) (rem 40 (rem 206 40))) 0) ; #f
      (rem 40 (rem 206 40))
      (gcd (rem (rem 206 40) (rem 40 (rem 206 40))) (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))))

(if (= (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))) 0) ; #t
      (rem (rem 206 40) (rem 40 (rem 206 40)))
      (gcd (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))) (rem (rem (rem 40 (rem 206 40))) (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))))

(rem (rem 206 40) (rem 40 (rem 206 40)))
(rem 6 (rem 40 6)) 
(rem 6 4)
2


; applicative-order eval
; 4 times
(gcd 206 40) 
(if (= 40 0) ; #f
      206 
      (gcd 40 (rem 206 40)))

(if (= 40 0) ; #f
      206 
      (gcd 40 6)) 

(if (= 6 0) ; #f
      40  
      (gcd 6 (rem 40 6)))

(if (= 6 0) ; #f
      40  
      (gcd 6 4)) 

(if (= 4 0) ; #f
      6   
      (gcd 4 (rem 6 4)))

(if (= 4 0) ; #f
      6   
      (gcd 4 2)) 

(if (= 2 0) ; #f
      4   
      (gcd 2 (rem 4 2)))

(if (= 2 0) ; #f
      4   
      (gcd 2 0)) 

(if (= 0 0) ; #t
      2   
      (gcd 2 0)) 

2
