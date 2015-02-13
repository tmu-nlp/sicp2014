#lang planet neil/sicp

(define (demo)
  (install-polynomial-package)
  (let ((p1 (make-polynomial 'x '((0 1) (5 1) (10 2))))
        (p2 (make-polynomial 'x '((0 13) (5 2) (2 10))))
        (pa (make-polynomial 'x '((2 2) (5 8))))
        (pb (make-polynomial 'x '((3 1) (7 4))))
        ;; (3y^2)(x^0) + (2y)x + (1y^0)x^2
        (ppa (make-polynomial 'x (list (list 0 (make-polynomial 'y '((2 3))))
                                       (list 1 (make-polynomial 'y '((1 2))))
                                       (list 2 (make-polynomial 'y '((0 1)))))))
        ;; (1y^0)(x^0) + (2y)x + (3y^2)x^2
        (ppb (make-polynomial 'x (list (list 0 (make-polynomial 'y '((0 1))))
                                       (list 1 (make-polynomial 'y '((1 2))))
                                       (list 2 (make-polynomial 'y '((2 3))))))))
    ((trace 'add add) p1 p2)
    ((trace 'mul mul) pa pb)
    ((trace 'add add) ppa ppb)
    ((trace 'sub sub) pa pb)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'add x (mul y -1)))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

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

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: ADD-POLY"
               (list p1 p2))))

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
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: ADD-POLY"
               (list p1 p2))))

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

  (define (poly-=zero? p)
    (all =zero? (map coeff (term-list p))))
  
  (define (combine-number-poly op n p)
    (let ((tn (make-scheme-number n)))
      (let ((p2 (make-constant-poly (variable p) tn)))
        (apply-generic op (tag p2) (tag p)))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))

  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))

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
  
  (put '=zero? '(polynomial)
       (lambda (term-list) (poly-=zero? term-list)))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))

  ;; (set! combine-number-poly (trace 'combine-number-poly combine-number-poly))
  
  'done)

(define (apply-generic op . args)
  (let* ((types (map type-tag args))
         (proc (get op types)))
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
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "No type tag" datum))))


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


(define tower '(scheme-number polynomial))

(define (compare-tower x y)
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
  (if (equal? (type-tag arg) target) arg
      (raise-to target (raise arg))))

(define (raise-list args)
  (let ((target (highest-type (map type-tag args))))
    (map (lambda (x) (raise-to target x)) args)))


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
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'make 'scheme-number (lambda (x) x))
  'done)

(define (show . args)
  ; debugging message.
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
  (if (null? list)
      #t
      (and (pred (car list)) (all pred (cdr list)))))


(install-polynomial-package)
(install-scheme-number-package)

;; (set! mul (trace 'mul mul))
;; (set! add (trace 'add add))
;; (set! apply-generic (trace 'apply-generic apply-generic))

