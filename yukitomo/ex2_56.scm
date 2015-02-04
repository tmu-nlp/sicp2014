(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

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
        (else
         (error "unknown expression type -- DERIV" exp))))

;(display (deriv '(+ x 3) 'x))
;(display "\n")
;(display (deriv '(* x y) 'x))
;(display "\n")
;(display (deriv '(* (* x y) (+ x 3)) 'x))
;(display "\n")

;update
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;被演算子の一方が0なら, make-sumはもう一方の被演算子を返す
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;0掛ける何かは0で, 1掛ける何かはそれ自身
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;(display (deriv '(+ x 3) 'x))
;(display "\n")
;(display (deriv '(* x y) 'x))
;(display "\n")
;(display (deriv '(* (* x y) (+ x 3)) 'x))

;ex2_56
;べき乗の微分 (** base exponent)をderivに組み込む

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (caddr e))
(define (exponent e) (cadr e))

(define (make-exponentiation base exponent)
  (cond  ((=number? base 0) 0)
         ((or (=number? base 1) (=number? exponent 0)) 1)
         ((=number? exponent 1) base)
         ((and (number? base) (number? exponent)) (expt base exponent))
         (else (list '** base exponent))))

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
         (if (variable? (exponent exp))
         (make-product
           (make-product
             (exponent exp)
             (make-exponentiation (base exp) (list '- (exponent exp) 1)))
           (deriv (base exp) var))
         (make-product
           (make-product
             (exponent exp) ;n
             (make-exponentiation (base exp) (make-sum (exponent exp) (- 1)))) ;u**(n-1)
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))


(display (deriv (make-exponentiation (list '+ 'x 1) 5) 'x))
(display "\n")
(display (deriv (make-exponentiation (list '+ (list '* 2 'x) 1) 'a) 'x))

