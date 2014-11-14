#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-27.scm

(define (square x) (* x x)) 

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (is-prime? n)
  (define (itr a)
    (cond ((= a 0) #t)
          (else (if (= (expmod a n n) a)
                    (itr (- a 1)) #f))))
  (if (itr (- n 1))
      (display "I'ts prime.")
      (display "I'ts not prime.")))




