#lang planet neil/sicp

(define (demo)
  (let* ((i2 (make-integer 2))
         (i4 (make-integer 4))
         (i5 (make-integer 5))
         (q3_5 (make-rational 3 5))
         (cri4i5 (make-complex-from-real-imag i4 i5))
         (cri2q3_5 (make-complex-from-real-imag i2 q3_5))
         (cpi5i4 (make-complex-from-mag-ang i5 i4))
         (cpq3_5i2 (make-complex-from-mag-ang q3_5 i2)))
    ((trace 'add add) cri4i5 cpi5i4)
    ((trace 'mul mul) cri4i5 cri2q3_5)
    ((trace 'mul mul) cpi5i4 cpq3_5i2)
    ((trace 'div div) cpq3_5i2 cpi5i4)))
                                      
(define (e^ x) (apply-generic 'e^ x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (re x) (apply-generic 're x))
(define (im x) (apply-generic 'im x))
(define (mag x) (apply-generic 'mag x))
(define (ang x) (apply-generic 'ang x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (square x) (mul x x))
(define (root x) (apply-generic 'root x))

(define (raise x) (apply-generic 'raise x))



                               
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



(define tower '(integer rational real complex))

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

(define (make-number x)
  (cond
   ((integer? x) (make-integer x))
   ((rational? x) (make-rational (numerator x) (denominator x)))
   ((real? x) (make-real x))
   ((complex? x) (make-complex-from-real-imag (real-part x) (imag-part x)))))

(define (make-integer x) ((get 'make 'integer) x))
(define (install-integer-package)
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (attach-tag 'integer (floor x)) ; Scheme integer つかってる
                       (error "Not an integer:" x))))

  (put 'e^ '(integer) (lambda (x) (make-real (exp x))))
  (put 'sine '(integer)  (lambda (x) (make-real (sin x))))
  (put 'cosine '(integer) (lambda (x) (make-real (cos x))))
  (put 'arctan '(integer integer) (lambda (x y) (make-real (atan x y))))
  (put 'root '(integer) (lambda (x) (make-real (sqrt x))))

  (put 'add '(integer integer)
       (lambda (x y) (make-integer (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (make-integer (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (make-integer (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (make-rational x y)))

  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (not (and (integer? n) (integer? d)))
        (error "Num and denom must be primitive integers"))
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
  (define (tag x) (attach-tag 'rational x))

  ;; interface to rest of the system
  (put 'e^ '(rational) (lambda (x) (make-real (- (exp (numer x)) (exp (denom x))))))
  (put 'cosine '(rational) (lambda (x) (make-real (cos (/ (numer x) (denom x))))))
  (put 'sine '(rational) (lambda (x) (make-real (sin (/ (numer x) (denom x))))))
  (put 'arctan '(rational rational)
       (lambda (x y)
         (make-real (atan (/ (numer x) (denom x)) (/ (numer y) (denom y))))))
  (put 'root '(rational) (lambda (x) (make-real (sqrt (/ (numer x) (denom x))))))

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))

  (put 'raise '(rational)
       (lambda (x) (make-real (exact->inexact (/ (numer x) (denom x))))))

  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-real x) ((get 'make 'real) x))
(define (install-real-package)
  (put 'make 'real
       (lambda (x) (if (real? x)
                  (attach-tag 'real (exact->inexact x))
                  (error "Not a primitive real:" x))))

  (put 'e^ '(real) (lambda (x) (make-real (exp x))))
  (put 'sine '(real) (lambda (x) (make-real (sin x))))
  (put 'arctan '(real real) (lambda (x y) (make-real (atan x y))))
  (put 'cosine '(real) (lambda (x) (make-real (cos x))))
  (put 'root '(real) (lambda (x) (make-real (sqrt x))))

  (put 'add '(real real)
       (lambda (x y) (make-real (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (make-real (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (make-real (* x y))))
  (put 'div '(real real)
       (lambda (x y) (make-real (* x y)))))


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag '(complex)) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang '(complex)) r a))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag '(rectangular)) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang '(polar)) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (re z1) (re z2))
                         (add (im z1) (im z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (add (re z1) (re z2))
                         (add (im z1) (im z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (add (mag z1) (mag z2))
                       (add (mag z1) (ang z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (add (mag z1) (ang z2))
                       (add (ang z1) (mag z2))))
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

  (put 'make-from-real-imag '(complex)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(complex)
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ; these just dispatch to the contained types
  (put 're '(complex) re)
  (put 'im '(complex) im)
  (put 'mag '(complex) mag)
  (put 'ang '(complex) ang)
  'done)

(define (install-rectangular-package)
  ;; internal procedures
  (define (re z) (car z))
  (define (im z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (root (add (square (re z))
               (square (im z)))))
  (define (angle z)
    (arctan (im z) (re z)))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 're '(rectangular) re)
  (put 'im '(rectangular) im)
  (put 'mag '(rectangular) magnitude)
  (put 'ang '(rectangular) angle)
  (put 'make-from-real-imag '(rectangular)
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  ;(set! angle (trace 'angle angle))
  (define (make-from-mag-ang r a) (cons r a))
  (define (re z) (mul (magnitude z) (cosine (angle z))))
  ;(set! re (trace 're re))
  (define (im z) (mul (magnitude z) (sine (angle z))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 're '(polar) re)
  (put 'im '(polar) im)
  (put 'mag '(polar) magnitude)
  (put 'ang '(polar) angle)
  (put 'make-from-mag-ang '(polar)
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;(install-scheme-number-package)
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-polar-package)
(install-rectangular-package)

;;(set! apply-generic (trace 'apply-generic apply-generic))
;;(set! raise-to (trace 'raise-to raise-to))
;;(set! raise (trace 'raise raise))


