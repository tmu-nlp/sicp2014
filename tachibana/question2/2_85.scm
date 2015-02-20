(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))


(define (install-rectangular-package)
   ;; 内部手続き
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

   ;; システムの他の部分とのインターフェース
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

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))

(define (install-complex-package)
   ;; 直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

   ;; 内部手続き
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
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))

   ;; システムの他の部分へのインターフェース
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

  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ-complex? z1 z2)))
  'done)


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))


(define (raise x) (apply-generic 'raise x))

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
  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  'done)

  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))



(define (install-rational-package)
   ;; 内部手続き
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

  (define (raise-rat x)
    (make-real (/ (* (numer x) 1.0) (denom x))))

  (define (equ-rat? x y)
    (and (= (numer x) (numer y)) (= (denom x) (denom y))))

   ;; システムの他の部分へのインターフェース
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  (put 'raise '(rational)
       (lambda (x) (raise-rat x)))

  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


;; 実数算術演算パッケージ
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '(real)
       (lambda (x) (= x 0.0)))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(install-rectangular-package)
(install-complex-package)
(install-scheme-number-package)
(install-rational-package)
(install-real-package)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
       (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags)))
                         (if (eq? type1 type2)
                             (error "E1. No method for these types" (list op type-tags))
                             (let ((coerced-args (coerce-higher-type args))) ;coerced-argsは型変換された後のargs
                                  (let ((proc (get op (map type-tag coerced-args))))
                                       (if proc
                                           (apply proc (map contents coerced-args))
                                           (error "E2.No method for these types" (list op type-tags)))))))
                    (error "E3. No method for these types" (list op type-tags)))))))


(define (equ? x y) (apply-generic 'equ? x y))



; ans
(define (install-project-package)
  (define (complex->real x)
    (make-real (real-part x)))
  (define (real->rational x)
    (make-rational (x->integer x) 1))
  (define (rational->integer x)
    (let ((n (car x))
          (d (cdr x)))
         (make-scheme-number (round (/ n d)))))
  (put 'project 'complex complex->real)
  (put 'project 'real real->rational)
  (put 'project 'rational rational->integer)
  'done)

(install-project-package)

(define (project x)
  (let ((proc (get 'project (type-tag x))))
       (if proc
           (proc (contents x))
           #f)))



(define (drop x)
  (if (pair? x)
      (let ((projected (project x)))
           (if projected
               (if (equ? (raise projected) x)
                   (drop projected)
                   x)
               x))
      x))



; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;        (let ((proc (get op type-tags)))
;             (if proc
;                 (drop (apply proc (map contents args))) ;; drop
;                 (if (= (length args) 2)
;                     (let ((type1 (car type-tags))
;                           (type2 (cadr type-tags)))
;                          (if (eq? type1 type2)
;                              (error "E1. No method for these types" (list op type-tags))
;                              (let ((coerced-args (coerce-higher-type args)))
;                                   (let ((proc (get op (map type-tag coerced-args))))
;                                        (if proc
;                                            (drop (apply proc (map contents coerced-args))) ;; drop
;                                            (error "E2.No method for these types" (list op type-tags)))))))
;                     (error "E3. No method for these types" (list op type-tags)))))))


; ex
(print (drop (make-scheme-number 2)))
(print (drop (make-rational 2 1)))
(print (drop (make-rational 2 3)))
(print (drop (make-real 3.0)))
(print (drop (make-real 2.5)))
(print (drop (make-complex-from-real-imag 2 0)))
(print (drop (make-complex-from-real-imag 2 3)))


