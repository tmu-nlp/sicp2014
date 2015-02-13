#lang planet neil/sicp

;;; Exercise 2.78

(define (make-scheme-number x) x)

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum: CONTENTS" datum))))

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  'done)


;;; Exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))
(define (install-equality-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (equal? x y)))
  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (equal? (real-part x) (real-part y))
              (equal? (imag-part x) (imag-part y)))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (equal? (numerator x) (numerator y))
              (equal? (denominator x) (denominator y))))))


;;; Exercise 2.80
(define (=zero? x)
  (apply-generic '=zero? x))
(define (install-zero-package)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put '=zero? '(complex) =zero?)
  (put '=zero? '(polar)
       (lambda (x) (= (magnitude
                  (attach-tag 'polar x))
                 0)))
  (put '=zero? '(rectangular)
       (lambda (x) (and (= (real-part x) 0)
                   (= (imag-part x) 0))))
  (put '=zero? '(rational)
       (lambda (x) (= (numerator x 0)))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-rational-package)
     ;; internal procedures
     (define (numer x) (car x))
     (define (denom x) (cdr x))
     (define (make-rat n d)
       (let ((g (gcd n d)))
         (cons (/ n g) (/ d g))))
     (define (add-rat x y)
       (make-rat (+ (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))
     (define (sub-rat x y)
       (make-rat (- (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))
     (define (mul-rat x y)
       (make-rat (* (numer x) (numer y))
                 (* (denom x) (denom y))))
     (define (div-rat x y)
       (make-rat (* (numer x) (denom y))
                 (* (denom x) (numer y))))
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

(define (install-complex-package)
  ;; imported procedures from rectagular and polar packages
  (define (make-from-real-imag x y)
       ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


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


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)
(install-equality-package)
(install-zero-package)
                                
(define (square x) (* x x))
