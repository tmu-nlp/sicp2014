#lang planet neil/sicp

;;; Exercise 2.94

;; Rather than signaling an error on GCD of disparate variables, this
;; continues the trick of imposing an order of variables from the last
;; problem. (Is GCD of disparate polynomials a coherent concept?)

;;As worked out by hand, gcd(x^4 - x^3 - 2x^2 + 2, x^3 - x) = -x^2 + x.

(define (demo)
  (let ((p1 (make-polynomial 'x '((10 2) (5 1) (0 1))))
        (p2 (make-polynomial 'x '((5 2) (2 10) (0 13))))
        (pa (make-polynomial 'x '((5 8) (2 2))))
        (pb (make-polynomial 'x '((7 4) (3 1))))
        ;; (3y^2)(x^0) + (2y)x + (1y^0)x^2
        (ppa (make-polynomial 'x (list (list 2 (make-polynomial 'y '((0 1))))
                                       (list 1 (make-polynomial 'y '((1 2))))
                                       (list 0 (make-polynomial 'y '((2 3)))))))
        ;; (1y^0)(x^0) + (2y)x + (3y^2)x^2
        (ppb (make-polynomial 'x (list (list 2 (make-polynomial 'y '((2 3))))
                                       (list 1 (make-polynomial 'y '((1 2))))
                                       (list 0 (make-polynomial 'y '((0 1)))))))
        (ppy (make-polynomial 'y '((1 1))))
        (rp1 (make-polynomial 'x '((2 1) (0 1))))
        (rp2 (make-polynomial 'x '((3 1) (0 1))))
        (gp1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
        (gp2 (make-polynomial 'x '((3 1) (1 -1)))))
    ((trace 'add add) p1 p2)
    ((trace 'mul mul) pa pb)
    ((trace 'add add) ppa ppb)
    ((trace 'sub sub) pa pb)
    ((trace 'div div)
     (make-polynomial 'x '((5 1) (0 -1)))
     (make-polynomial 'x '((2 1) (0 -1))))
    ((trace 'mul mul) ppb ppy)
    (let ((rf (make-rational rp2 rp1)))
      ((trace 'add add) rf rf))
    ((trace 'greatest-common-divisor greatest-common-divisor)
     gp1 gp2)))

                                        ;                    GENERIC FUNCTIONS

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'add x (mul y -1)))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (var x) (apply-generic 'var x))
(define (terms x) (apply-generic 'terms x))
(define (raise x) (apply-generic 'raise x))
(define (wrap x) (apply-generic 'wrap x))
(define (greatest-common-divisor n d) (apply-generic 'greatest-common-divisor n d))


                                        ;                    POLYNOMIAL PACKAGE

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    ;;assert that terms descend in order
    (if (not (descending-terms? term-list))
        (error "terms are not in descending order" term-list))
    (if (not (permissible-nesting? variable term-list))
        (error "Polynomials improperly nested" term-list))        
    (cons variable term-list))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (descending-terms? term-list)
    (cond ((null? term-list) #t)
          ((null? (cdr term-list)) #t)
          ((<= (order (car term-list))
               (order (cadr term-list))) #f)
          (else (descending-terms? (cdr term-list)))))
  
  (define (make-constant-poly var const)
    (make-poly var (list (list 0 const))))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (permissible-nesting? variable term-list)
    (define (permissible-nested-term? variable term)
      (let ((nested-var (var (coeff term)))
            (nested-terms (terms (coeff term))))
        (if nested-var
            (if (symbol<? variable nested-var)
                (all (curl permissible-nested-term? nested-var) nested-terms)
                #f)
            #t)))
    (all (curl permissible-nested-term? variable) term-list))

  (define (wrap-poly var poly)
    (make-poly var (list (list 0 (tag poly)))))

  (define (wrapping-op op)
    (lambda (p1 p2)
      (let ((wrap-which
             (if (same-variable? (variable p1) (variable p2)) 'neither
                 (if (symbol<? (variable p1) (variable p2))
                     'second 'first))))
        (let ((wp1 (case wrap-which
                     ((first) (wrap-poly (variable p2) p1))
                     ((second neither) p1)))
              (wp2 (case wrap-which
                     ((first neither) p2)
                     ((second) (wrap-poly (variable p1) p2)))))
          (make-poly (variable wp1)
                     (op (term-list wp1) (term-list wp2)))))))
  (define (add-poly p1 p2) ((wrapping-op add-terms) p1 p2))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-poly p1 p2) ((wrapping-op mul-terms) p1 p2))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (sub-terms L1 L2)
    (add-terms L1 (mul-terms L2 '((0 -1)))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((divided (div-terms (term-list p1)
                                  (term-list p2))))
          (list (make-poly
                 (variable p1)
                 (car divided))
                (make-poly
                 (variable p1)
                 (cadr divided))))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1) ; reimainder
              (let* ((new-c (div (coeff t1) (coeff t2)))
                     (new-o (- (order t1) (order t2)))
                     (quotient-term-list (list  (make-term new-o new-c)))
                     (term-remainder (sub-terms L1 (mul-terms quotient-term-list L2)))
                     (rest-of-result (div-terms term-remainder L2))
                     (full-quotient (add-terms quotient-term-list (car rest-of-result))))
                (list full-quotient (cadr rest-of-result)))))))

  (define (poly-=zero? p)
    (all =zero? (map coeff (term-list p))))

  (define (gcd-poly a b) ((wrapping-op gcd-terms) a b))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))

  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  
  (define (combine-number-poly op n p)
    ;; Elevate a number to a compatible polynomial,
    ;; then retag both and combine generically.
    (let ((tn (make-scheme-number n)))
      (let ((p2 (make-constant-poly (variable p) tn)))
        (apply-generic op (tag p2) (tag p)))))

  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))

  (put 'var '(polynomial) variable)

  (put 'terms '(polynomial) term-list)

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put-commutative
   'add 'scheme-number 'polynomial
   (lambda (n p) (combine-number-poly 'add n p)))
  
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put-commutative
   'mul 'scheme-number 'polynomial
   (lambda (n p) (combine-number-poly 'mul n p)))

  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (map tag result))))
  
  (put '=zero? '(polynomial)
       (lambda (term-list) (poly-=zero? term-list)))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))

  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (a b) (tag (gcd-poly a b))))

  (put 'wrap '(variable polynomial) wrap-poly)

  'done)

                                        ;                     APPLY GENERIC
(define (apply-generic op . args)
  (let* ((types (map type-tag args))
         (proc (get op types)))
    ;;(show " types ->" types "proc ->" proc)
    (if proc
        (apply proc (map contents args))
        (let* ((raised (raise-list args))
               (raised-types (map type-tag raised))
               (proc (get op raised-types)))
          (if proc
              (apply proc (map contents raised))
              (error "No method or raised method found" (list op types)))))))

(define (attach-tag type-tag contents)
  (cond ((equal? type-tag 'scheme-number) contents)
        ((equal? type-tag 'variable) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((symbol? datum) 'variable)
        ((pair? datum) (car datum))
        (else (error "No type tag" datum))))


                                        ;                     DISPATCH TABLES
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum: CONTENTS" datum))))

(define (update table key value)
  (cond ((null? table)
         (cons (cons key value) nil))
        ((equal? (caar table) key)
         (cons (cons (caar table) value)
               (cdr table)))
        (else (cons (car table) (update (cdr table) key value)))))

(define (lookup table key)
  (cond ((null? table) nil)
        ((equal? (caar table) key)
         (cdar table))
        (else
         (lookup (cdr table) key))))

(define type-tables '())

(define (get table key)
  (let ((result (lookup (lookup type-tables table) key)))
    (if (null? result) #f result)))

(define (put table key value)
  (set! type-tables
        (update type-tables table
                (update (lookup type-tables table) key value))))

(define (put-commutative op type1 type2 proc)
  (put op (list type1 type2) proc)
  (put op (list type2 type1) (lambda (x y) (proc y x))))


                                        ;                     NUMERIC TOWER
(define tower '(scheme-number polynomial))

(define (compare-tower x y)
  ;;return 1 if type y is higher, -1 if type x is higher, 0 if equal
  (define (step-up-tower ranks)
    (if (null? ranks)
        (error "Types not on tower" (list x y))
        (let ((rank (car ranks)))
          (if (equal? x rank)
              (if (equal? y rank)
                  'same
                  'second)
              (if (equal? y rank)
                  'first
                  (step-up-tower (cdr ranks)))))))
  (step-up-tower tower))

(define (highest-type list)
  (define (pick-highest x y)
    (case (compare-tower x y)
      ((first) x)
      ((second) y)
      ((same) x)))
  (fold-left pick-highest (car tower) list))

(define (raise-to target arg)
  ;; repeatedly raise the argument until it is at the required type.
  (if (equal? (type-tag arg) target) arg
      (raise-to target (raise arg))))

(define (raise-list args)
  (let ((target (highest-type (map type-tag args))))
    (map (lambda (x) (raise-to target x)) args)))

                                        ;                    RATIONAL TYPE


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d) (cons n d))
  
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


                                        ;                    NUMBER TYPE
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (install-scheme-number-package)
  (put 'e^ '(scheme-number) exp)
  (put 'cosine '(scheme-number) cos)
  (put 'sine '(scheme-number) sin)
  (put 'arcsin '(scheme-number) sin)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (a b) (gcd a b)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'make 'scheme-number (lambda (x) x))
  (put 'var '(scheme-number) (lambda (x) #f))
  (put 'terms '(scheme-number) (lambda (x) nil))

  'done)


                                        ;                     DEBUGGING/UTIL
(define (show . args)
  ;; write a debugging message.
  (cond ((null? args)
         (display "\n"))
        ((string? (car args))
         (display (car args))
         (display " ")
         (apply show (cdr args)))
        (else
         (write (car args))
         (display " ")
         (apply show (cdr args)))))

(define (trace name fun)
  ;; Wrap a function with a tracer.
  (lambda args
    (display "(")
    (display (symbol->string name))
    (display " ")
    (map (lambda (x) (write x) (display " ")) args)
    (display "\n")
    (let ((result (apply fun args)))
      (display " => ")
      (write result)
      (display ")\n")
      result)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (all pred list)
  ;; apply predicate to each item; return #t if all results are non-#f.
  (if (null? list)
      #t
      (and (pred (car list)) (all pred (cdr list)))))

(define (curl proc . args)
  (lambda more-args (apply proc (append args more-args))))

(define (curr proc . args)
  (lambda more-args (apply proc (append more-args args))))

(define (symbol<? x y)
  (string<? (symbol->string x) (symbol->string y)))

                                        ;                    INSTALLING / TRACING

;; (set! apply-generic (trace 'apply-generic apply-generic))

(install-polynomial-package)
(install-scheme-number-package)
(install-rational-package)

;; (set! mul (trace 'mul mul))
;; (set! add (trace 'add add))
;; (set! apply-generic (trace 'apply-generic apply-generic))




