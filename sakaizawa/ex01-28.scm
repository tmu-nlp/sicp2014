i#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex01-22.scm

(define (mod a b) (remainder a b))

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((tmp (mod (square (expmod2 base (/ exp 2) m)) m)))
           (if (and (< 1 tmp) (< tmp m) (= (mod (square tmp) m) 1))
               (display (list base exp tmp (mod (square tmp) m)))) tmp))
        (else (mod (* base (expmod2 base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (tt a i)
    (cond ((= a n) i)
          ((> (expmod2 a (- n 1) n) 1) (tt (+ a 1) (+ i 1)))
          (else (tt (+ a 1) i))))
  (tt 1 0))

(define (fermat-test2 n)
  (define (try-it a)
    (= (expmod2 a n n) a))
    (try-it (+ 1 (random(- n 1)))))
(define (fast-prime2? n times)
  (cond ((= times 0) #t)
        ((fermat-test2 n) (fast-prime2? n (- times 1)))
        (else #f)))


