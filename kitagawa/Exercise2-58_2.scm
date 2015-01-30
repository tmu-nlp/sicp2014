(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (augend s) (caddr s))

(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (define (deriv-ori exp)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv-ori (addend exp))
                     (deriv-ori (augend exp))))
          ((product? exp)
           (make-sum
            (make-product (multiplier exp)
                          (deriv-ori (multiplicand exp)))
            (make-product (deriv-ori (multiplier exp))
                          (multiplicand exp))))
          (else
           (error "unknown expression type -- DERIV" exp))))
  ;; 入力された値を括弧で囲むようにしてから微分を行う
  (deriv-ori (transform exp)))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (addend s) (car s))

(define (multiplier p) (car p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (transform exp)
  (cond ((<= (length exp) 3) exp)
        ((or (sum? exp) (product? exp))
         (list (car exp)
               (cadr exp)
               (transform (cddr exp))))))


;;; 使用例
(deriv '(x + 3 * (x + y + 2)) 'x)       ; 4
(deriv '(3 + 5 * x) 'x)                 ; 5
(deriv '(3 * x + 5) 'x)                 ; 3
(deriv '(3 * 5 * x) 'x)                 ; 15
