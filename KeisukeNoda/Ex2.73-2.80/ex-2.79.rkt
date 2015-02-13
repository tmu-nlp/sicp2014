#lang racket

(define h (make-hash))
(define (put op type element)
  (dict-set! h (list op type) element))
(define (get op type)
  (dict-ref h (list op type) false))


(define (attach-tag type-tag contents)
  (if (number? contents) contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Incorrectly marked data -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Incorrectly marked data -- CONTENTS" datum))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for this type -- APPLY-GENERIC"
           (list op type-tags))))))

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
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(apply-generic 'equ? (make-scheme-number 5) (make-scheme-number 2))
(apply-generic 'equ? (make-scheme-number 5) (make-scheme-number 5))


;  (put 'equ? '(rational rational) (lambda (x y) (eq-rational? x y)))
;  (put 'equ? '(complex complex) (lambda (x y) (eq-complex? x y)))