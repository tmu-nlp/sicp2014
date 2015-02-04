(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))


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
             (exponent exp)
             (make-exponentiation (base exp) (- (exponent exp) 1)))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))


(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base s) (car s))

(define (exponent s) (caddr s))

(define (make-exponentiation m1 m2)
  (cond ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (expt m1 m2))
        (else (list m1 '** m2))))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cddr p)))

;ans
;2_58aから(augend s)と(multiplicand p)の(cons '+ (cddr s))の部分を(cddr s)のようにする

(print (deriv '(x + 3) 'x))

(print (deriv '(x + 3 * (x * y + 2)) 'x))

(print (deriv '(3 * (x + y + 2)) 'x))

; しかし、+と*の演算の順序を上の変更ではしていない。またするとしてもderivの抽象構造自体を変更
; する必要があると思われるので、この問題は解けないと考えられる。



















