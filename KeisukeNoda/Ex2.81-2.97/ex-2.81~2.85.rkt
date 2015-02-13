#lang planet neil/sicp

; "apply-generic"
(define apply-generic nil)

;;; Exercise 2.81

(define (exp x y) (apply-generic 'exp x y))

(define (install-louis-package)
  (define (scheme-number->scheme-number n)
    (display "scheme-number->scheme-number called\n")
    n)
  (define (complex->complex z)
    (display "complex->complex called\n")
    z)

  (put-coercion 'scheme-number
                'scheme-number
                scheme-number->scheme-number)
  (put-coercion 'complex 'complex complex->complex)
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (expt x y))))

(define (infinite-coercer)
  (let ((old-tables coercion-tables)
        (apply-generic-old apply-generic))
       (install-louis-package)
       (exp 3 3)
       (exp (make-complex-from-real-imag (3 4))
            (make-complex-from-real-imag (0.5 1)))
       (set! coercion-tables old-tables)))


(define (apply-generic-louis op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond
       (proc
        (apply proc (map contents args)))
       ((and (= (length args) 2))
        (let ((type1 (car type-tags))
              (type2 (cadr type-tags))
              (a1 (car args))
              (a2 (cadr args)))
          (if (not (equal? type1 type2))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond
                 (t1->t2 (apply-generic op (t1->t2 a1) a2))
                 (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                 (else (error "No method for these types: "
                              (list op type-tags)))))
              (error "No method for these types: "
                     (list op type-tags)))))
       (else (error
              "No method for these types: "
              (list op type-tags)))))))

(define (no-infinite-coercer)
  (let ((old-tables coercion-tables)
        (apply-generic-old apply-generic))
       (install-louis-package)
       (set! apply-generic apply-generic-louis)
       (exp 3 3)
       (exp (make-complex-from-real-imag 3 4)
            (make-complex-from-real-imag 0.5 1))
       (set! coercion-tables old-tables)
       (set! apply-generic apply-generic-old)))

;;; Exercise 2.82

(define (coerce data to)
  (if (equal? (type-tag data) to) data
      ((get-coercion (type-tag data) to) data)))

(define (try-list pred list)
  (if (null? list) #f
      (or (and (pred (car list)) (car list))
          (try-list pred (cdr list)))))

(define (all pred list)
  (if (null? list)
      #t
      (and (pred (car list)) (all pred (cdr list)))))

(define (coercion-exists? from to)
  (if (equal? from to) #t (get-coercion from to)))

(define (coercions-exist? from-list to)
  (all (lambda (from) (coercion-exists? from to)) from-list))

(define (find-coercion from-list to-list)
  (try-list (lambda (to) (coercions-exist? from-list to)) to-list))

(define (apply-generic-casting-n op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((coerce-to (find-coercion type-tags type-tags)))
          (if coerce-to
              (let ((coerced-args
                     (map (lambda (arg) (coerce arg coerce-to)) args)))
                (apply apply-generic (cons op coerced-args)))
              (error "No method for these types: " (list op type-tags)))))))

;;; Exercise 2.83


(define (make-integer x) ((get 'make '(integer)) x))
(define (install-integer-package)
  (put 'make '(integer)
       (lambda (x) (if (integer? x)
                  (attach-tag 'integer (floor x)) ;Scheme integer つかってる
                  (error "Not an integer:" x))))
  (put 'add '(integer integer)
       (lambda (x y) (make-integer (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (make-integer (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (make-integer (* x y))))
  (put 'equ? '(integer integer) equal?))


(define (make-real x) ((get 'make '(real)) x))
(define (install-real-package)
  (put 'make '(real)
       (lambda (x) (if (real? x)
                  (attach-tag 'real (exact->inexact x))
                  (error "Not a real number:" x))))
  (put 'add '(real real)
       (lambda (x y) (make-real (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (make-real (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (make-real (* x y))))
  (put 'equ? '(real real) equal?))


(define (raise x) (apply-generic-raising 'raise x))
(define (install-raisers-package)
  (define (integer->rational x) (make-rational x 1))
  (put 'raise '(integer) integer->rational)
  (put-coercion 'integer 'rational integer->rational)
  (define (rational->real x) (make-real (/ (car x) (cdr x))))
  (put 'raise '(rational) rational->real)
  (put-coercion 'rational 'real rational->real)
  (define (real->complex x) (make-complex-from-real-imag x 0.0))
  (put 'raise '(real) real->complex)
  (put-coercion 'real 'complex 'real->complex)
  'done)

;;; Exercise 2.84

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

(define (apply-generic-raising op . args)
  (let* ((types (map type-tag args))
         (proc (get op types)))
    (if proc
        (apply proc (map contents args))
        (let* ((raised (raise-list args))
               (raised-types (map type-tag raised))
               (proc (get op raised-types)))
          (if proc
              (apply proc (map contents raised))
              (error "No method and no casting" (list op types)))))))

;;; Exercise 2.85

;; (add (make-complex-from-real-imag 3.25 0)
;;      (make-complex-from-mag-ang 2.75 0))

(define (project x) (apply-generic-raising 'project x))

(define (install-project-package)
  (put 'project '(rational)
       (lambda (x) (make-integer (quotient (car x) (cdr x)))))
  (put 'project '(real)
       (lambda (x) (make-integer (inexact->exact (floor x)))))
  (put 'project '(complex)
       (lambda (x) (make-real (real-part x)))))

(define (drop x)

  (let* ((type (type-tag x))
         (proj (get 'project (list type))))
    (if proj
        (begin
          (if (equal? 'first (compare-tower type (car tower)))
              (let ((lowered (project x)))
                (if (equ? lowered x)
                    (drop lowered)
                    x))))
        x)))

(define (apply-generic-dropping op . args)
  (let ((result (apply apply-generic-raising (cons op args))))
    (let ((dropped (drop result)))
      dropped)))
                        
(define (show . args)
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

(define coercion-tables '())

(define (put-coercion from to proc)
  (set! coercion-tables
        (update coercion-tables from
                (update (lookup coercion-tables from)
                        to proc))))

(define (get-coercion from to)
  (let ((result (lookup (lookup coercion-tables from) to)))
    (if (null? result) #f result)))

(define (install-coercions-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (put-coercion 'scheme-number
                'complex
                scheme-number->complex)
  'done)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))


(define (apply-generic-casting op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond
       (proc
        (apply proc (map contents args)))
       ((and (= (length args) 2))
        (let ((type1 (car type-tags))
              (type2 (cadr type-tags))
              (a1 (car args))
              (a2 (cadr args)))
          (let ((t1->t2 (get-coercion type1 type2))
                (t2->t1 (get-coercion type2 type1)))
            (cond
             (t1->t2 (apply-generic op (t1->t2 a1) a2))
             (t2->t1 (apply-generic op a1 (t2->t1 a2)))
             (else (error "No method for these types: "
                          (list op type-tags)))))))
       (else (error
              "No method for these types: "
              (list op type-tags)))))))

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

(define (make-scheme-number x) x)

(define (attach-tag type-tag contents)
  (cond ((equal? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else 'nonpair))) 
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
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (define (->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (put-coercion 'scheme-number 'complex ->complex)
  'done)

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
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numerator x 0))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectagular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag '(rectangular)) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang '(polar)) r a))
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
  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex) =zero?)
  (put 'make-from-real-imag '(complex)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(complex)
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag '(complex)) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang '(complex)) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

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
  (put 'make-from-real-imag '(rectangular)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(rectangular)
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put '=zero? '(rectangular)
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
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
  (put '=zero? '(polar)
       (lambda (x) (= (magnitude
                       (attach-tag 'polar x)) ;ugh
                      0)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag '(polar)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(polar)
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)
(install-coercions-package)
(install-integer-package)
(install-real-package)
(install-raisers-package)
(install-project-package)
(set! apply-generic apply-generic-dropping)
                               
(define (square x) (* x x))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
