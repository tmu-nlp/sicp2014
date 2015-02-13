#lang racket

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))


;; exponentiation?
;; ↓ base
;;  ↓ exponent
;;   ↓ make-exponentiation (**)
;;    ↓ '(** x 5) -> '(* 5 (** x 4))

;構成子
(define (make-exponentiation base exponent)
  ;簡約化
  (cond ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        (else (list '** base exponent))))
;述語
(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

;選択子(基数)
(define (base x)
  (cadr x))

;選択子(乗数)
(define (exponent x)
  (caddr x))

; --------------------
;この微分の規則を、deriv に組み込み.
;condに追加.

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)   ; 新しい節を追加
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
         (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(** x 5) 'x)
'(* 5 (** x 4))

(deriv '(** x 1) 'x)
1

(deriv '(** x 2) 'x)
'(* 2 x)

(deriv '(* (** x 5) x) 'x)
'(+ (** x 5) (* (* 5 (** x 4)) x))