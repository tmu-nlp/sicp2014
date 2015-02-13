#lang racket

; [b]
;  [a]で作成したプログラムの改良
;  不要な括弧を省く
;  乗算は加算より優先する
;  

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (exists? item x)
  (cond ((null? x) false)
      ((eq? item (car x)) true)
      (else (exists? item (cdr x)))))

(define (before? item x)
  (if (or (null? x) (eq? item (car x))) 
      '()
      (cons 
       (car x)
       (before? item (cdr x)))))

(define (make-flat x)
  (cond ((= (length x) 1) (car x))
        (else x)))

(define (sum? x)
  (and (pair? x) (exists? '+ x)))

(define (addend s) (make-flat (before? '+ s)))

(define (augend s) (make-flat (cdr (memq '+ s))))

(define (product? x)
  (and (pair? x) (not (sum? x)) (exists? '* x)))

(define (multiplier p) (make-flat (before? '* p)))

(define (multiplicand p) (make-flat (cdr (memq '* p))))

;; exponentiation?
;; ↓ base
;;  ↓ exponent
;;   ↓ make-exponentiation (**)
;;    ↓ '(** x 5) -> '(* 5 (** x 4))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        (else (list '** base exponent))))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))
; ------------------------

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
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))


; run

(sum? '((2 + 3) * 2 + (x + 2))) ; -> #t
(sum? '((2 + 3) * 2 * (x + 2))) ; -> #f
(newline)

(before? '+ '((2 + 3) * 2 + 3)) ; -> '((2 + 3) * 2)
(addend '((2 + 3) * 2 + (x + 2))) ; -> '((2 + 3) * 2)
(augend '((2 + 3) * 2 + (x + 2))) ; -> '(x + 2)
(deriv '(3 * x * 5) 'x) ; -> 15
