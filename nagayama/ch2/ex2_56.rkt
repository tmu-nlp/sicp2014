#lang racket

; 微分手続きの拡張
; べき乗に対応（** を冪乗演算子として使用）
; 
; 本文のプログラムに書き足した箇所には "#" を付けた

(define (deriv exp var)
(cond ((number? exp) 0)
      ((variable? exp)
       (if (same-variable? exp var) 1 0))
      ((sum? exp)
       (make-sum (deriv (addend exp) var)
                 (deriv (angend exp) var)))
      ((product? exp)
       (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
      ((exponentiation? exp) ; --#
       (make-product
        (exponent exp)
        (make-exponentiation (base exp)
                             (make-sum (exponent exp) -1)))
       (deriv (base exp) var))
      (else
       (error "unknown expression type: DERIV" exp))))

; units
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (make-exponentiation e1 e2) (list '** e1 e2)) ; --#

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (angend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; --#--
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

; run
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (** x 3) y) 'x)
(deriv '(** x 3) 'x)

