#lang planet neil/sicp

(define (update table key value)
  (cond ((null? table)
         (cons (cons key value) nil))
        ((equal? (caar table) key)
         (cons (cons (caar table) value)
               (cdr table)))
        (else (cons (car table) (update (cdr table) key value)))))

(define (lookup table key)
  (cond ((null? table)
         nil)
        ((equal? (caar table) key)
         (cdar table))
        (else
         (lookup (cdr table) key))))

(define type-tables '())

(define (get table key)
  (lookup (lookup type-tables table) key))

(define (put table key value)
  (set! type-tables
        (update type-tables table
                (update (lookup type-tables table) key value))))

;;; Exercise 2.74

(define engineering
  '(engineering
    ((name . "Komachi") (salary . 1000000))
    ((salary . 50000) (name . "Noda"))))

(define hr
  '(hr
    ("Yamaguchi" 350000) ("Ishikawa" 320000) ("OKubo" 50000)))

;; インターフェイス - accounts payable:

(define (install-engineering-department)
  
  (define (my-get-field rec field)
    (cond ((null? rec) nil)
          ((equal? (caar rec) field) (cdar rec))
          (else (my-get-field (cdr rec) field))))
  
  (define (my-get-salary rec)
    (my-get-field rec 'salary))
  (put 'get-salary '(engineering-record) my-get-salary)

  (define (my-get-record data name)
    (cond ((null? data) nil)
          ((equal? (my-get-field (car data) 'name) name)
           (attach-tag 'engineering-record (car data)))
          (else (my-get-record (cdr data) name))))
  (put 'get-record '(engineering) my-get-record)

  'done)
(install-engineering-department)

;; インターフェイス　— HR:

(define (install-hr-department)
  (define (my-get-record data name)
    (cond ((null? data) nil)
          ((equal? (caar data) name)
           (attach-tag 'hr-record (car data)))
          (else (my-get-record (cdr data) name))))
  (put 'get-record '(hr) my-get-record)

  (define my-get-salary cadr)
  (put 'get-salary '(hr) my-get-salary)
  
  'done)
(install-hr-department)

;; A.

(define (get-record file name)
  ((get 'get-record (list (type-tag file))) (contents file) name))


;; B.
(define (get-salary record)
  ((get 'get-salary (list (type-tag record))) (contents record)))

;; C.
(define (find-employee-record files name)
  (if (null? files) '()
      (let ((rec (get-record (car files) name)))
        (if (null? rec)
            (find-employee-record (cdr files) name)
            rec))))

;; D.
;; 同様に書く。


;;; type tagging
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (write "type-tags: ")
    (write type-tags)
    (write "\n")
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (list (car exp)))
(define (operands exp) (cdr exp))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

;;; constructors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

;(define (make-from-mag-ang r a)
;  ((get 'make-from-mag-ang 'polar) r a))