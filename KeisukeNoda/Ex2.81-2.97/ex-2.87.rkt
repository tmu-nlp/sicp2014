#lang racket
(define h (make-hash))
(define (put op type element)
  (dict-set! h (list op type) element))
(define (get op type)
  (dict-ref h (list op type) false))
;-------------------------------------------------
(define h2 (make-hash))
(define (put-coercion op type element)
  (dict-set! h2 (list op type) element))
(define (get-coercion op type)
  (dict-ref h2 (list op type) false))
;-------------------------------------------------
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
;-------------------------------------------------
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
;------------------------------------------------
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types"
                 (list op type-tags))))))
;--------------------------------------------------
(define (install-polynomial-package)
  ;; internal procedures, representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
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
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
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
        (error "Polys not in same var -- MUL-POLY"
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
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p) (empty-termlist? (term-list p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
;---------------------------------------------------------
(install-scheme-number-package)
(install-polynomial-package)

;---------------------------------------------------------
;; sample data from internet
(define ca (make-polynomial 'x '((2 2) (1 3) (0 1))))
(define cb (make-polynomial 'x '((3 4) (1 1))))
(define cc (make-polynomial 'x '((1 1) (0 10))))
(define cd (make-polynomial 'x '((3 -4) (1 -1))))
(define p1 (make-polynomial 'y (list (list 2 ca)
                                     (list 1 cb)
                                     (list 0 cc))))
(define p2 (make-polynomial 'y (list (list 1 ca)
                                     (list 0 cb))))
(define p3 (make-polynomial 'y (list (list 2 ca)
                                     (list 1 cd)
                                     (list 0 cc))))



(add ca cb)
;Value: (polynomial x (3 4) (2 2) (1 4) (0 1))

(add cb cd)
;Value: (polynomial x)

(=zero? (add cb cd))
;Value: #t

(mul ca cc)
;Value: (polynomial x (3 2) (2 23) (1 31) (0 10))

(add p1 p2)
;Value: (polynomial y (2 (polynomial x (2 2) (1 3) (0 1)))
;                     (1 (polynomial x (3 4) (2 2) (1 4) (0 1)))
;                     (0 (polynomial x (3 4) (1 2) (0 10))))

(add p1 p3)
;Value: (polynomial y (2 (polynomial x (2 4) (1 6) (0 2)))
;                     (0 (polynomial x (1 2) (0 20))))

(mul p2 p3)
;Value: (polynomial y (3 (polynomial x (4 4) (3 12) (2 13) (1 6) (0 1)))
;                     (1 (polynomial x (6 -16) (4 -8) (3 2) (2 22) (1 31) (0 10)))
;                     (0 (polynomial x (4 4) (3 40) (2 1) (1 10))))
