#lang planet neil/sicp

;;; Exercise 2.90

;; Aside from defining a generic 'polynomial' type that delegated to
;; the enclosed type, the colution to this involved defining 'add' and
;; 'mul' procedures that worked between dense and sparse polynomials.
;; This in turn relied on converting dense to sparse polynomials
;; (which is the easier of the two conversions, though space
;; efficiency might motivate being able to do it either way depending
;; on how large the result would end up.)

;; The "raise" and numeric tower infrastructure is largely unused.

(define (demo)
  (let ((p1 (make-polynomial-sparse 'x '((0 1) (5 1) (10 2))))
        (p2 (make-polynomial-sparse 'x '((0 13) (5 2) (2 10))))
        (pa (make-polynomial-sparse 'x '((2 2) (5 8))))
        (pb (make-polynomial-sparse 'x '((3 1) (7 4))))
        ;; (3y^2)(x^0) + (2y)x + (1y^0)x^2
        (ppa (make-sparse-polynomial
              'x (list (list 0 (make-polynomial-sparse 'y '((2 3))))
                       (list 1 (make-polynomial-sparse 'y '((1 2))))
                       (list 2 (make-polynomial-sparse 'y '((0 1)))))))
        ;; (1y^0)(x^0) + (2y)x + (3y^2)x^2
        (ppb (make-sparse-polynomial
              'x (list (list 0 (make-polynomial-sparse 'y '((0 1))))
                       (list 1 (make-polynomial-sparse 'y '((1 2))))
                       (list 2 (make-polynomial-sparse 'y '((2 3)))))))
        (dp1 (make-polynomial-dense
              'x '(4 3 2 1)))
        (dp2 (make-polynomial-dense
              'x '(10 20 30))))
    ((trace 'add add) p1 p2)
    ((trace 'mul mul) pa pb)
    ((trace 'add add) ppa ppb)
    ((trace 'sub sub) pa pb)
    ((trace 'add add) dp1 dp2)
    ((trace 'mul mul) dp1 dp2)
    ((trace 'add add) p1 dp2)
    ((trace 'mul mul) dp1 p2)))

                                        ;                    GENERIC FUNCTIONS

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'add x (mul y -1)))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))


                                        ;                    POLYNOMIAL PACKAGES

(define (make-polynomial-sparse var terms)
  ((get 'make-sparse 'polynomial) var terms))

(define (make-polynomial-dense var terms)
  ((get 'make-dense 'polynomial) var terms))

;this converts from dense to sparse polynomials.
(define (make-sparse p) (apply-generic 'make-sparse p))

(define (install-polynomial-package)

  (define (tag p) (attach-tag 'polynomial p))

  (put 'make-sparse 'polynomial
       (lambda (var terms) (tag ((get 'make 'sparse-polynomial) var terms))))
  (put 'make-dense 'polynomial
       (lambda (var terms) (tag ((get 'make 'dense-polynomial) var terms))))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add p1 p2))))
  
  (put-commutative
   'add 'scheme-number 'polynomial
   (lambda (n p) (tag (add (make-scheme-number n)) p)))

  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul p1 p2))))

  (put-commutative
   'mul 'scheme-number 'polynomial
   (lambda (n p) (tag (mul (make-scheme-number n) p))))

  (put '=zero? '(polynomial) 
       (lambda (p) (=zero? p)))

  'done)



(define (make-dense-polynomial var terms)
  ((get 'make 'dense-polynomial) var terms))

(define (install-dense-polynomial-package)
  ;; internal representation
  ;; there is a list of coefficients, starting with 0
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (make-constant-poly var const)
    (make-poly var (list const)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: ADD-POLY"
               (list p1 p2))))

  (define (add-terms L1 L2) ;add-fill fills out short lists.
    (map-fill add (list L1 L2) '(0 0)))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: ADD-POLY"
               (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (null? L1)
        (the-empty-termlist)
        ;;(ax^0+bx^1+...)*(mx^1+nx^2+...) ==
        ;;a*(mx^0+nx^1+...)+(bx^1+cx^2)*
        (let ((first (map-fill mul
                               (list '() L2)
                               (list (first-term L1) nil))))
          (add-terms first (mul-terms (rest-terms L1) (cons 0 L2))))))

  (define (poly-=zero? p)
    (all =zero? (term-list p)))
  
  (define (combine-number-poly op n p)
    ;; Elevate a number to a compatible polynoqmial,
    ;; then retag both and combine with a given generic.
    (let ((tn (make-scheme-number n)))
      (let ((p2 (make-constant-poly (variable p) tn)))
        (apply-generic op (tag p2) (tag p)))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'dense-polynomial p))

  (define (make-sparse p)
    (let ((order-list (count (length (term-list p))))
          (coef-list (term-list p)))
     (make-sparse-polynomial (variable p)
                             (map list coef-list order-list))))

  (put-commutative
   'add 'dense-polynomial 'sparse-polynomial
   (lambda (d s) (add
             (make-sparse d)
             (attach-tag 'sparse-polynomial s))))

  (put-commutative
   'mul 'dense-polynomial 'sparse-polynomial
   (lambda (d s)
     (mul
      (make-sparse d)
      (attach-tag 'sparse-polynomial s))))

  (put 'make 'dense-polynomial
       (lambda (var term-list) (make-poly var term-list)))

  (put 'add '(dense-polynomial dense-polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put-commutative
   'add 'scheme-number 'dense-polynomial
   (lambda (n p) (combine-number-poly 'add n p)))
  
  (put 'mul '(dense-polynomial dense-polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put-commutative
   'mul 'scheme-number 'dense-polynomial
   (lambda (n p) (combine-number-poly 'mul n p)))
  
  (put '=zero? '(dense-polynomial)
       (lambda (term-list) (poly-=zero? term-list)))
  
  (put 'make 'dense-polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))

  (put 'make-sparse '(dense-polynomial) make-sparse)
  
  'done)

(define (make-sparse-polynomial var terms)
  ((get 'make 'sparse-polynomial) var terms))

(define (install-sparse-polynomial-package)
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
    ;; Elevate a number to a compatible sparse-polynomial,
    ;; then retag both and combine generically.
    (let ((tn (make-scheme-number n)))
      (let ((p2 (make-constant-poly (variable p) tn)))
        (apply-generic op (tag p2) (tag p)))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse-polynomial p))

  (put 'make 'sparse-polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))

  (put 'add '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put-commutative
   'add 'scheme-number 'sparse-polynomial
   (lambda (n p) (combine-number-poly 'add n p)))
  
  (put 'mul '(sparse-polynomial sparse-polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put-commutative
   'mul 'scheme-number 'sparse-polynomial
   (lambda (n p) (combine-number-poly 'mul n p)))
  
  (put '=zero? '(sparse-polynomial)
       (lambda (term-list) (poly-=zero? term-list)))
  (put 'make 'sparse-polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))

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
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
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
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'make 'scheme-number (lambda (x) x))
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

(define (map-fill fun arg-lists fills)
  ;;"map" but extend any short lists in arg-lists with corresponding fills.
  ;;e.g. (map-fill-accum '+ '((1 2 3) (3)) '(0 4)) => (4 6 7)

  (define (fcar x fill)
    (if (null? x) fill (car x)))

  (define (fcdr x)
    (if (null? x) nil
        (cdr x)))

  (define (map-fill-accum fun arg-lists fills out)
    (if (all null? arg-lists)
        out
        (let ((args (map fcar arg-lists fills)))
          (let ((element (apply fun args)))
            (map-fill-accum
             fun (map fcdr arg-lists) fills
             (cons element out))))))
  
  (reverse (map-fill-accum fun arg-lists fills nil)))

(define (count n)
  (define (count-inner n accum)
    (if (< n 0) accum
        (count-inner (- n 1) (cons n accum))))
  (count-inner (- n 1) nil))


                                        ;                    INSTALLING / TRACING

(install-polynomial-package)
(install-sparse-polynomial-package)
(install-scheme-number-package)
(install-dense-polynomial-package)

;; (set! mul (trace 'mul mul))
;; (set! add (trace 'add add))
;; (set! apply-generic (trace 'apply-generic apply-generic))


